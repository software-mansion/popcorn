%% The following function checks if function with more than 8 lines of matches 
%% is causing an error ("Unexpected nbits value").
-module(long_function).
-export([start/0]).

start() ->
  User = "Jose",
  User = "Jose",
  User = "Jose",
  User = "Jose",
  User = "Jose",
  User = "Jose",
  User = "Jose",
  User = "Jose",
  User = "Jose".
