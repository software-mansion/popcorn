%
% This file is part of AtomVM.
% ETS based implementation of persistent_term.
%
% Copyright 2024 Tomasz Sobkiewicz <tomasz.sobkiewicz@swmansion.com>
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
-module(persistent_term).
-export([get/2, put/2]).

get(Key, Default) ->
    try ets:lookup(persistent_term, Key) of
        Result ->
            case Result of
                [{Key, Res}] -> Res;
                _ -> Default
            end
    catch
        error:badarg ->
            ets:new(persistent_term, [set, public, named_table]),
            Default
    end.

put(Key, Value) ->
    try ets:insert(persistent_term, {Key, Value}) of
        Result -> Result
    catch
        error:badarg ->
            ets:new(persistent_term, [set, public, named_table]),
            ets:insert(persistent_term, {Key, Value})
    end.
