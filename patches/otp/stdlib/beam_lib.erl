-module(beam_lib).


-record(bb, {pos = 0 :: integer(),
  bin :: binary(),
  source :: binary() | string()}).

-compile({popcorn_patch_private, pread/3}).

pread(FD, AtPos, Size) ->
  #bb{pos = Pos, bin = Binary} = FD,
  Skip = AtPos-Pos,
  case Binary of
%%    Patch reason: 
%%    The pattern matching binary with < Skip size to:
%%    "<<_:Skip/binary, B:Size/binary, Bin/binary>>" 
%%    causes a crash for yet unspecified reasons.
%%    Patch:
    Bin0 when byte_size(Bin0) < Skip ->
      {FD, eof};
%%    End of patch
    <<_:Skip/binary, B:Size/binary, Bin/binary>> ->
      NFD = FD#bb{pos = AtPos+Size, bin = Bin},
      {NFD, {ok, B}};
    <<_:Skip/binary, Bin/binary>> when byte_size(Bin) > 0 ->
      NFD = FD#bb{pos = AtPos+byte_size(Bin), bin = <<>>},
      {NFD, {ok, Bin}};
    _ ->
      {FD, eof}
  end.
