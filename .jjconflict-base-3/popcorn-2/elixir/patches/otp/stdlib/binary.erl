%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% Copyright 2024 Yuto Oguchi <oguchiyuto@realglobe.jp>, Realglobe Inc.
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%%-----------------------------------------------------------------------------
%% @doc An implementation of a subset of the Erlang/OTP binary interface.
%% @end
%%-----------------------------------------------------------------------------
-module(binary).

-export([
    at/2,
    decode_hex/1,
    encode_hex/1, encode_hex/2,
    part/3,
    split/2, split/3,
    compile_pattern/1,
    matches/2,
    decode_unsigned/1, decode_unsigned/2,
    encode_unsigned/1, encode_unsigned/2
]).

%%-----------------------------------------------------------------------------
%% @param   Binary binary to get a byte from
%% @param   Index  0-based index of the byte to return
%% @returns value of the byte from the binary
%% @doc     Get a byte from a binary by index.
%% @end
%%-----------------------------------------------------------------------------
-spec at(Binary :: binary(), Index :: non_neg_integer()) -> byte().
at(_Binary, _Index) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data hex encoded binary to decode
%% @returns decoded binary
%% @doc     Decodes a hex encoded binary into a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec decode_hex(Data :: <<_:_*16>>) -> binary().
decode_hex(Data) ->
    case byte_size(Data) rem 2 of
        0 -> <<<<(binary_to_integer(B, 16))>> || <<B:2/binary>> <= Data>>;
        _ -> erlang:error(badarg)
    end.

%%-----------------------------------------------------------------------------
%% @param   Data binary data to convert into hex encoded binary
%% @returns hex encoded binary
%% @doc     Encodes a binary into a hex encoded binary using the specified case for the hexadecimal digits "a" to "f".
%% @end
%%-----------------------------------------------------------------------------
-spec encode_hex(Data :: binary()) -> binary().
encode_hex(Data) ->
    encode_hex(Data, uppercase).

%%-----------------------------------------------------------------------------
%% @param   Data binary data to convert into hex encoded binary
%% @param   Case which case to encode into
%% @returns hex encoded binary
%% @doc     Encodes a binary into a hex encoded binary using the specified case for the hexadecimal digits "a" to "f".
%% @end
%%-----------------------------------------------------------------------------
-spec encode_hex(Data :: binary(), Case :: lowercase | uppercase) -> binary().
encode_hex(Data, uppercase) ->
    <<(integer_to_binary(B, 16)) || <<B:4>> <= Data>>;
encode_hex(Data, lowercase) ->
    <<<<(hd(string:to_lower(integer_to_list(B, 16)))):8>> || <<B:4>> <= Data>>.

%%-----------------------------------------------------------------------------
%% @param   Binary binary to extract a subbinary from
%% @param   Pos    0-based index of the subbinary to extract
%% @param   Len    length, in bytes, of the subbinary to extract.
%% @return a subbinary from Binary
%% @doc Get the part of a given binary.
%% A negative length can be passed to count bytes backwards.
%% @end
%%-----------------------------------------------------------------------------
-spec part(Binary :: binary(), Pos :: non_neg_integer(), Len :: integer()) -> binary().
part(_Binary, _Pos, _Len) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @equiv split(Binary, Pattern, [])
%% @param   Binary  binary to split
%% @param   Pattern pattern to perform the split
%% @return a list composed of one or two binaries
%% @doc Split a binary according to pattern.
%% If pattern is not found, returns a singleton list with the passed binary.
%% Unlike Erlang/OTP, pattern must be a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec split(Binary :: binary(), Pattern :: binary()) -> [binary()].
split(_Binary, _Pattern) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Binary  binary to split
%% @param   Pattern pattern to perform the split
%% @param   Options options for the split
%% @return a list composed of one or two binaries
%% @doc Split a binary according to pattern.
%% If pattern is not found, returns a singleton list with the passed binary.
%% Unlike Erlang/OTP, pattern must be a binary.
%% Only implemented option is `global'
%% @end
%%-----------------------------------------------------------------------------
-spec split(Binary :: binary(), Pattern :: binary(), Option :: [global]) -> [binary()].
split(_Binary, _Pattern, _Option) ->
    erlang:nif_error(undefined).

%% Patch reason: String.split is using patterns compiled by compile_pattern/1
%% AtomVM does not provide such feature. Luckily all the functions that use compiled pattern
%% also use just a binary pattern.
%% In OTP the compiled pattern is a tuple and for now we are not supporting it - in every
%% binary module function elixir wasm will support only binary patterns.
-spec compile_pattern(Pattern) -> binary() when
    Pattern :: binary() | [binary()].
compile_pattern(Pattern) ->
    Pattern.

%% Patch reason: NIF missing in AtomVM
matches(Binary, Patterns) ->
    do_matches(Binary, Patterns, 0, []).

do_matches(Binary, Patterns, Offset, Result) ->
    case binary:match(Binary, Patterns) of
        {Start, Length} ->
            End = Start + Length,
            <<_Processed:End/binary, CutBinary/binary>> = Binary,
            do_matches(CutBinary, Patterns, End + Offset, [{Start + Offset, Length} | Result]);
        nomatch ->
            lists:reverse(Result)
    end.

% Patch reason: encode_unsigned and decode_unsigned NIFs not available in AtomVM
% These functions should also accept 'little' as the second argument,
% but the 'little' modifier for binary matching / creation is unsupported in AtomVM.

encode_unsigned(Unsigned) ->
  BitSize = erlang:ceil(math:log2(Unsigned)),
  Size = erlang:ceil(BitSize / 8) * 8,
  <<Unsigned:Size>>.

encode_unsigned(Unsigned, big) ->
  encode_unsigned(Unsigned).

decode_unsigned(Binary) ->
    Size = erlang:byte_size(Binary) * 8,
    <<Unsigned:Size>> = Binary,
    Unsigned.

decode_unsigned(Binary, big) ->
    decode_unsigned(Binary).
