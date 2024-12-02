%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

%% Generic compiler options, passed from the erl_compile module.

-record(options,
    % Include paths (list of
    {
        includes = [] :: [file:filename()],
        % absolute directory names).

        % Directory for result
        outdir = "." :: file:filename(),
        % (absolute path).

        % Type of output file.
        output_type = undefined :: atom(),
        % Preprocessor defines.  Each
        defines = [] :: [atom() | {atom(), _}],
        % element is an atom
        % (the name to define), or
        % a {Name, Value} tuple.

        % Warning level (0 - no
        warning = 1 :: non_neg_integer(),
        % warnings, 1 - standard level,
        % 2, 3, ... - more warnings).

        % Verbose (true/false).
        verbose = false :: boolean(),
        % Optimize options.
        optimize = 999,
        % Compiler specific options.
        specific = [] :: [_],
        % Name of output file (internal
        outfile = "" :: file:filename(),
        % use in erl_compile.erl).

        % Current working directory
        cwd :: file:filename()
        % for erlc.
    }
).
