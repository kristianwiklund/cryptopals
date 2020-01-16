-module(decryptsix).
-export([decryptsix/0]).

decryptsix() ->
    Rawdata = utils:readfile("data1_6.txt"),
    Text=lists:flatten(lists:map(fun(X) ->
					 base64:decode_to_string(X) 
				 end, Rawdata)),
    lists:flatten(utils:decrypt_running_xor(Text)).

    
