-module(init).

-export([get_arguments/0, get_argument/1]).

get_arguments() -> [].
get_argument(_Arg) -> error.