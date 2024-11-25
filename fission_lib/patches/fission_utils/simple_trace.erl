%% File generated automatically with gen_trace.exs
%% Do not edit
-module(simple_trace).

-export([trace/4, trace/5, trace/6, trace/7, trace/8, trace/9, trace/10, trace/11, trace/12, trace/13, trace/14, trace/15, trace/16, trace/17, trace/18, trace/19, trace/20, trace/21, trace/22, trace/23, trace/24]).

trace(M, F, File, Line) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/0 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/0 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/1 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/1 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/2 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/2 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/3 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/3 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/4 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/4 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/5 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/5 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/6 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/6 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/7 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/7 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/8 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/8 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/9 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/9 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/10 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/10 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/11 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/11 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/12 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/12 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/13 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/13 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/14 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/14 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/15 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/15 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/16 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/16 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/17 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/17 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/18 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/18 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/19 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/19 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20) ->
    console:print([erlang:pid_to_list(self()), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/20 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20),
    console:print([erlang:pid_to_list(self()), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/20 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.

