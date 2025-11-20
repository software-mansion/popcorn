-module(io_lib_format).

-export([fwrite_g/1]).

% Patch reason: the 'short' option used in OTP
% isn't supported by AtomVM
-spec fwrite_g(float()) -> string().
fwrite_g(Float) ->
    float_to_list(Float, [compact, {decimals, 16}]).
