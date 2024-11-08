-module(dupa_trace).

-export([dupa_trace/2, dupa_trace/3, dupa_trace/4, dupa_trace/5, dupa_trace/6, dupa_trace/7, dupa_trace/8, dupa_trace/9, dupa_trace/10, dupa_trace/11, dupa_trace/12, dupa_trace/13, dupa_trace/14, dupa_trace/15, dupa_trace/16, dupa_trace/17, dupa_trace/18, dupa_trace/19, dupa_trace/20, dupa_trace/21, dupa_trace/22, dupa_trace/23, dupa_trace/24, dupa_trace/25, dupa_trace/26, dupa_trace/27]).

dupa_trace(M, F ) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/0\n"]),
    R = M:F(),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/0\n"]),
    R.
dupa_trace(M, F, A) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/1\n"]),
    R = M:F(A),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/1\n"]),
    R.
dupa_trace(M, F, A,B) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/2\n"]),
    R = M:F(A,B),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/2\n"]),
    R.
dupa_trace(M, F, A,B,C) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/3\n"]),
    R = M:F(A,B,C),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/3\n"]),
    R.
dupa_trace(M, F, A,B,C,D) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/4\n"]),
    R = M:F(A,B,C,D),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/4\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/5\n"]),
    R = M:F(A,B,C,D,E),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/5\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/6\n"]),
    R = M:F(A,B,C,D,E,F),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/6\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/7\n"]),
    R = M:F(A,B,C,D,E,F,G),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/7\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/8\n"]),
    R = M:F(A,B,C,D,E,F,G,H),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/8\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/9\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/9\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/10\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/10\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/11\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/11\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/12\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/12\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/13\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/13\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/14\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/14\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/15\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/15\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/16\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/16\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/17\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/17\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/18\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/18\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/19\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/19\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/20\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/20\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/21\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/21\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/22\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/22\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/23\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/23\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/24\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/24\n"]),
    R.
dupa_trace(M, F, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/25\n"]),
    R = M:F(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/25\n"]),
    R.

