-module(beam_lib).
-export([checksum/1]).

checksum(File0) ->
    case catch checksum_1(File0) of
        {ok, _} = Result -> Result;
        Error -> Error
    end.

checksum_1(File0) ->
    File = popcorn_module:beam_filename(File0),
    Beam = read_beam(File),
    {ok, Module, _} = popcorn_module:scan_beam(Beam, []),
    case erts_internal:beamfile_module_checksum(Beam) of
        Digest when is_binary(Digest) ->
            {ok, {Module, {xxh3_128, Digest}}};
        undefined ->
            popcorn_module:error({not_a_beam_file, File})
    end.

read_beam(Beam) when is_binary(Beam) ->
    popcorn_module:maybe_uncompress(Beam);
read_beam(File) ->
    case file:read_file(File) of
        {ok, Beam} -> popcorn_module:maybe_uncompress(Beam);
        Error -> popcorn_module:file_error(File, Error)
    end.
