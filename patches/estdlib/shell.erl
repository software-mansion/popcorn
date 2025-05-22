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
-module(shell).

-export([start/2]).

-compile({popcorn_patch_private, non_builtin_local_func/4}).
-compile({popcorn_patch_private, initiate_records/2}).
-compile({popcorn_patch_private, read_records/2}).
-compile({popcorn_patch_private, find_file/1}).

-define(LINEMAX, 30).
-define(CHAR_MAX, 60).
-define(DEF_HISTORY, 20).
-define(DEF_RESULTS, 20).
-define(DEF_CATCH_EXCEPTION, false).
-define(DEF_PROMPT_FUNC, default).
-define(DEF_STRINGS, true).

-define(RECORDS, shell_records).

-define(MAXSIZE_HEAPBINARY, 64).
-record(shell_state,{
                     bindings = [],
                     records = [],
                     functions = []
                    }).

start(NoCtrlG, StartSync) ->
%%    _ = code:ensure_loaded(user_default),
    Ancestors = [self() | case get('$ancestors') of
                              undefined -> [];
                              Anc -> Anc
                          end],
    spawn(fun() ->
                  put('$ancestors', Ancestors),
                  popcorn_module:server(NoCtrlG, StartSync)
          end).

%% Patch reason: erlang:function_exported/3 not implemented in AtomVM
non_builtin_local_func(F,As,Bs, FT) ->
%%    Arity = length(As),
%%    case erlang:function_exported(user_default, F, Arity) of
%%        true ->
%%            {eval,erlang:make_fun(user_default, F, Arity),As,Bs};
%%        false ->
  popcorn_module:shell_default(F,As,Bs, FT).
%%    end.

%% Patch reason: even if the module is loaded in AtomVM it does not mean that 
%% the path to the file is correct as we operate in WASM in Browser.
%% Also - user_default module is never loaded in AtomVM.
initiate_records(Bs, RT) ->
    RNs1 = popcorn_module:init_rec(shell_default, Bs, RT),
%%    RNs2 = case code:is_loaded(user_default) of
%%               {file,_File} ->
%%                   init_rec(user_default, Bs, RT);
%%               false ->
%%                   []
%%           end,
    RNs2 = [],
    lists:usort(RNs1 ++ RNs2).

%% Patch reason: files are not working in Popcorn
read_records(FileOrModule, Opts0) ->
    Opts = lists:delete(report_warnings, Opts0),
    case find_file(FileOrModule) of
        {beam, Beam, File} -> 
          popcorn_module:read_records_from_beam(Beam, File);
        {files,[File]} ->
            {error, avm_not_supporting_files};
%%            read_file_records(File, Opts);
        {files,Files} ->
            {error, avm_not_supporting_files};
%%            lists:flatmap(fun(File) ->
%%                                  case read_file_records(File, Opts) of
%%                                      RAs when is_list(RAs) -> RAs;
%%                                      _ -> []
%%                                  end
%%                          end, Files);
        Error ->
            Error
    end.

%% Patch reason: files are not working in Popcorn
find_file(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
        File when is_list(File) ->
            %% Special cases:
            %% - Modules not in the code path (loaded with code:load_abs/1):
            %%   code:get_object_code/1 only searches in the code path
            %%   but code:which/1 finds all loaded modules
            %% - File can also be a file in an archive,
            %%   beam_lib:chunks/2 cannot handle such paths but
            %%   erl_prim_loader:get_file/1 can
%%            case erl_prim_loader:get_file(File) of
%%                {ok, Beam, _} ->
%%                    {beam, Beam, File};
%%                error ->
%%                    {error, nofile}
%%            end;
            {error, nofile};
        preloaded ->
            {_M, Beam, File} = code:get_object_code(Mod),
            {beam, Beam, File};
        _Else -> % non_existing, interpreted, cover_compiled
            {error,nofile}
    end;
find_file(File) ->
    case catch filelib:wildcard(File) of
        {'EXIT',_} ->
            {error,invalid_filename};
        Files ->
            {files,Files}
    end.
