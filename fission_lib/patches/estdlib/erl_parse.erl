%% -*- erlang -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%% Definition of the Erlang grammar.

-module(erl_parse).

-export([first_anno/1]). % Internal export.

%% Use the fact that fold_anno() visits nodes from left to right.
%% Could be a bit slow on deeply nested code without column numbers
%% even though only the left-most branch is traversed.
first_anno(Abstract) ->
    Anno0 = element(2, Abstract),
    F = fun(Anno, Anno1) ->
                Loc = erl_anno:location(Anno),
                Loc1 = erl_anno:location(Anno1),
                case loc_lte(Loc, Loc1) of
                    true ->
                        Anno;
                    false ->
                        throw(Anno1)
                end
        end,
	try erl_parse:fold_anno(F, Anno0, Abstract)
	catch
		Error -> Error
	end.

loc_lte(Line1, Location2) when is_integer(Line1) ->
  loc_lte({Line1, 1}, Location2);
loc_lte(Location1, Line2) when is_integer(Line2) ->
  loc_lte(Location1, {Line2, 1});
loc_lte(Location1, Location2) ->
  Location1 =< Location2.