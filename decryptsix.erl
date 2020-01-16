-module(decryptsix).
-export([decryptsix/0]).

decryptsix() ->
    Rawdata = utils:readfile("data1_6.txt"),
    Text=lists:flatten(lists:map(fun(X) ->
		      base64:decode_to_string(X) 
			    end, Rawdata)),
    {Length,_} = lists:nth(1,utils:findkeysize(Text, 2,40)),
    Chunks = utils:shufflechunks(Text,Length),
    Candidates = lists:map(fun(X)->
		      utils:decryptxor(X)
			   end, Chunks),
    lists:nth(1,lists:reverse(lists:keysort(1,Candidates))).
    
