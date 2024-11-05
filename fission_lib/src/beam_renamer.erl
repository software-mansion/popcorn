%%%-------------------------------------------------------------------
%%% @doc
%%% Rename a module which is already compiled.
%%%
%%% The idea behind `beam_renamer` is to be able to load an erlang module
%%% (which is already compiled) under a different name.  Normally, there's
%%% an error message if one does that:
%%%
%%%     1> {x, Bin, _} = code:get_object_code(x).
%%%     {x,<<...>>,...}
%%%     2> code:load_binary(y, "y.beam", Bin).
%%%     {error,badfile}
%%%
%%%     =ERROR REPORT==== 8-Nov-2009::22:01:24 ===
%%%     Loading of y.beam failed: badfile
%%%
%%%     =ERROR REPORT==== 8-Nov-2009::22:01:24 ===
%%%     beam/beam_load.c(1022): Error loading module y:
%%%       module name in object code is x
%%%
%%% This is where `beam_renamer` comes in handy.  It'll rename the module
%%% by replacing the module name *within* the beam file.
%%%
%%%     1> {x, Bin0, _} = code:get_object_code(x).
%%%     {x,<<...>>,...}
%%%     2> Bin = beam_renamer:rename(Bin0, y).
%%%     <<...>>
%%%     2> code:load_binary(y, "y.beam", Bin).
%%%     {module,y}
%%%
%%% @author Klas Johansson (erlang@klasjohansson.se)
%%% @end
%%%-------------------------------------------------------------------
-module(beam_renamer).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([rename/2]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

%% according to spec below
-define(beam_num_bytes_alignment, 4).

%% In order to load a module under a different name, the module name
%% has to be changed within the beam file itself.  The following code
%% snippet does just that.  It's based on a specification of the beam
%% format (a fairly old one, from March 1 2000, but it seems there are
%% not changes changes which affect the code below):
%%
%%      http://www.erlang.se/~bjorn/beam_file_format.html
%%
%% BEWARE of modules which refer to themselves!  This is where things
%% start to become interesting...  If ?MODULE is used in a function
%% call, things should be ok (the module name is replaced in the
%% function call).  The same goes for a ?MODULE which stands on its
%% own in a statement (like the sole return value).  But if it's
%% embedded for example within a tuple or list with only constant
%% values, it's added to the constant pool which is a separate chunk
%% within the beam file.  The current code doesn't replace occurences
%% within the constant pool.  Although possible, I'll leave that for
%% later. :-)
%%
%% The rename function does two things: It replaces the first atom of
%% the atom table (since apparently that's where the module name is).
%% Since the new name may be shorter or longer than the old name, one
%% might have to adjust the length of the atom table chunk
%% accordingly.  Finally it updates the top-level form size, since the
%% atom table chunk might have grown or shrunk.
%%
%% From the above beam format specification:
%%
%%     This file format is based on EA IFF 85 - Standard for
%%     Interchange Format Files. This "standard" is not widely used;
%%     the only uses I know of is the IFF graphic file format for the
%%     Amiga and Blorb (a resource file format for Interactive Fiction
%%     games). Despite of this, I decided to use IFF instead of
%%     inventing my of own format, because IFF is almost right.
%%
%%     The only thing that is not right is the even alignment of
%%     chunks. I use four-byte alignment instead. Because of this
%%     change, Beam files starts with 'FOR1' instead of 'FORM' to
%%     allow reader programs to distinguish "classic" IFF from "beam"
%%     IFF. The name 'FOR1' is included in the IFF document as a
%%     future way to extend IFF.
%%
%%     In the description of the chunks that follow, the word
%%     mandatory means that the module cannot be loaded without it.
%%
%%
%%     FORM HEADER
%%
%%     4 bytes    'FOR1'  Magic number indicating an IFF form. This is an
%%                        extension to IFF indicating that all chunks are
%%                        four-byte aligned.
%%     4 bytes    n       Form length (file length - 8)
%%     4 bytes    'BEAM'  Form type
%%     n-8 bytes  ...     The chunks, concatenated.
%%
%%
%%     ATOM TABLE CHUNK
%%
%%     The atom table chunk is mandatory. The first atom in the table must
%%     be the module name.
%%
%%     4 bytes    'Atom'  chunk ID
%%     4 bytes    size    total chunk length
%%     4 bytes    n       number of atoms
%%     xx bytes   ...     Atoms. Each atom is a string preceeded
%%                        by the length in a byte.
%%
%% The following section about the constant pool (literal table) was
%% reverse engineered from the source (beam_lib etc), since it wasn't
%% included in the beam format specification referred above.
%%
%%     CONSTANT POOL/LITERAL TABLE CHUNK
%%
%%     The literal table chunk is optional.
%%
%%     4 bytes    'LitT'  chunk ID
%%     4 bytes    size    total chunk length
%%     4 bytes    size    size of uncompressed constants
%%     xx bytes   ...     zlib compressed constants
%%
%%     Once uncompressed, the format of the constants are as follows:
%%
%%     4 bytes    size    unknown
%%     4 bytes    size    size of first literal
%%     xx bytes   ...     term_to_binary encoded literal
%%     4 bytes    size    size of next literal
%%     ...

%%--------------------------------------------------------------------
%% @spec rename(BeamBin0, NewName) -> BeamBin
%%         BeamBin0 = binary()
%%         BeamBin = binary()
%%         NewName = atom()
%% @doc Rename a module.  `BeamBin0' is a binary containing the
%% contents of the beam file.
%% @end
%%--------------------------------------------------------------------
rename(BeamBin0, Name) ->
    Name0 = get_module_name(BeamBin0),
    NameBin = atom_to_binary(Name, utf8),
    BeamBin1 = replace_in_atab(BeamBin0, NameBin),
    BeamBin = replace_in_const_pool(BeamBin1, Name0, Name),
    update_form_size(BeamBin).

%% Replace the first atom of the atom table with the new name
get_module_name(<<"AtU8", _CnkSz:32, _NumAtoms:32, NameSz:8, Name:NameSz/binary, _Rest/binary>>) ->
    binary_to_atom(Name, utf8);
get_module_name(<<_, Rest/binary>>) ->
    get_module_name(Rest).

%% Replace the first atom of the atom table with the new name
replace_in_atab(<<"AtU8", CnkSz0:32, Cnk:CnkSz0/binary, Rest/binary>>, Name) ->
    <<NumAtoms:32, NameSz0:8, _Name0:NameSz0/binary, CnkRest/binary>> = Cnk,
    NumPad0 = num_pad_bytes(CnkSz0),
    <<_:NumPad0/unit:8, NextCnks/binary>> = Rest,
    NameSz = size(Name),
    CnkSz = CnkSz0 + NameSz - NameSz0,
    NumPad = num_pad_bytes(CnkSz),
    <<"AtU8", CnkSz:32, NumAtoms:32, NameSz:8, Name:NameSz/binary, CnkRest/binary, 0:NumPad/unit:8,
        NextCnks/binary>>;
replace_in_atab(<<C, Rest/binary>>, Name) ->
    <<C, (replace_in_atab(Rest, Name))/binary>>.

%% Replace all occurences of the atom in the constant pool
replace_in_const_pool(
    <<"LitT", CnkSz0:32, Cnk0:CnkSz0/binary, Rest/binary>>,
    Name0,
    Name
) ->
    <<_UncompSz0:32, Consts0/binary>> = Cnk0,
    NumPad0 = num_pad_bytes(CnkSz0),
    <<_:NumPad0/unit:8, NextCnks/binary>> = Rest,
    <<N:32, Pool0/binary>> = zlib:uncompress(Consts0),
    Pool = replace_in_const_pool_1(Pool0, Name0, Name),
    UncompBin = <<N:32, Pool/binary>>,
    Consts = zlib:compress(UncompBin),
    UncompSz = size(UncompBin),
    Cnk = <<UncompSz:32, Consts/binary>>,
    CnkSz = size(Cnk),
    NumPad = num_pad_bytes(CnkSz),

    <<"LitT", CnkSz:32, Cnk:CnkSz/binary, 0:NumPad/unit:8, NextCnks/binary>>;
replace_in_const_pool(<<C, Rest/binary>>, Name0, Name) ->
    <<C, (replace_in_const_pool(Rest, Name0, Name))/binary>>;
replace_in_const_pool(<<>>, _Name0, _Name) ->
    <<>>.

replace_in_const_pool_1(<<BSz0:32, B0:BSz0/binary, Rest/binary>>, Name0, Name) ->
    B = term_to_binary(replace_in_const_term(binary_to_term(B0), Name0, Name)),
    BSz = size(B),
    <<BSz:32, B:BSz/binary, (replace_in_const_pool_1(Rest, Name0, Name))/binary>>;
replace_in_const_pool_1(<<>>, _Name0, _Name) ->
    <<>>.

replace_in_const_term(T, Name0, Name) when is_list(T) ->
    map_improper_list(
        fun(SubT) -> replace_in_const_term(SubT, Name0, Name) end,
        T
    );
replace_in_const_term(T, Name0, Name) when is_tuple(T) ->
    list_to_tuple(replace_in_const_term(tuple_to_list(T), Name0, Name));
replace_in_const_term(T, Name0, Name) when T =:= Name0 ->
    Name;
replace_in_const_term(T, _Name0, _Name) ->
    T.

%% Calculate the number of padding bytes that have to be added for the
%% BinSize to be an even multiple of ?beam_num_bytes_alignment.
num_pad_bytes(BinSize) ->
    case ?beam_num_bytes_alignment - (BinSize rem ?beam_num_bytes_alignment) of
        4 -> 0;
        N -> N
    end.

%% Update the size within the top-level form
update_form_size(<<"FOR1", _OldSz:32, Rest/binary>> = Bin) ->
    Sz = size(Bin) - 8,
    <<"FOR1", Sz:32, Rest/binary>>.

map_improper_list(_F, []) -> [];
map_improper_list(F, [H | T]) -> [F(H) | map_improper_list(F, T)];
map_improper_list(F, Term) -> F(Term).
