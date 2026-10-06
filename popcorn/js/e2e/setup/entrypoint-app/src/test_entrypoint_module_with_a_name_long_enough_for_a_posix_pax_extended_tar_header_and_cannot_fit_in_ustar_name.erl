-module(test_entrypoint_module_with_a_name_long_enough_for_a_posix_pax_extended_tar_header_and_cannot_fit_in_ustar_name).
-export([answer/0]).

answer() ->
    42.
