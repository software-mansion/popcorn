
-module(file).
-export([get_cwd/0, open/2]).

%% data types
-type filename()  :: string().
-type posix() ::
        'eacces' | 'eagain' |
        'ebadf' | 'ebadmsg' | 'ebusy' |
        'edeadlk' | 'edeadlock' | 'edquot' |
        'eexist' |
        'efault' | 'efbig' | 'eftype' |
        'eintr' | 'einval' | 'eio' | 'eisdir' |
        'eloop' |
        'emfile' | 'emlink' | 'emultihop' |
        'enametoolong' | 'enfile' |
        'enobufs' | 'enodev' | 'enolck' | 'enolink' | 'enoent' |
        'enomem' | 'enospc' | 'enosr' | 'enostr' | 'enosys' |
        'enotblk' | 'enotdir' | 'enotsup' | 'enxio' |
        'eopnotsupp' | 'eoverflow' |
        'eperm' | 'epipe' |
        'erange' | 'erofs' |
        'espipe'  | 'esrch'  | 'estale' |
        'etxtbsy' |
        'exdev'.
%%% BIFs

-export([native_name_encoding/0]).

-spec native_name_encoding() -> latin1 | utf8.
native_name_encoding() ->
    latin1.

%%% End of BIFs

%%%-----------------------------------------------------------------
%%% File server functions.
%%% Functions that do not operate on a single open file.
%%% Stateless.
-spec get_cwd() -> {ok, Dir} | {error, Reason} when
      Dir :: filename(),
      Reason :: posix().

get_cwd() ->
    prim_file:get_cwd(). %Patch reason: Needed to use nif directly.

% Patch reason: this dummy implementation is sufficient
% for some calls not to break
open(_File, _Opts) -> {error, not_implemented}.