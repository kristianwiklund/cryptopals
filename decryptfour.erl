-module(decryptfour).
-export([doit/0]).

doit() ->
    Input=utils:readfile("data1_4.txt"),
    T=lists:map(fun(X)->
		      utils:decryptxor(X)
		end, Input),
    T2 = lists:keysort(1, T),
    T3 = lists:reverse(T2),
    lists:nth(1,T3).
