-module(utils).
-include_lib("eunit/include/eunit.hrl").
-export([hex2b64/1,fxor/2,decryptxor/1]).

fxor(From, What) ->
    T1 = list_to_integer(From, 16),
    T2 = list_to_integer(What, 16),
    integer_to_list(T1 bxor T2, 16).


fxor_test() ->
    P = integer_to_list(list_to_integer("746865206B696420646f6E277420706C6179",16),16),
    K = fxor("1c0111001f010100061a024b53535009181c","686974207468652062756c6c277320657965"),
    P = K.

freq() ->
    lists:reverse("ETAOINSRHDLUCMFYWGPBVKXQJX").

h2s([A,B|Hex], S)->
    h2s(Hex,[list_to_integer([A,B],16)|S]);
h2s(_,S) ->
    S.
h2s(Hex) ->
    lists:reverse(h2s(Hex,"")).

score(String) ->
    P = freq(),
    lists:foldl(fun(C,Acc) ->
			if C == 0 ->
				0; % there cannot be any zero in a string
			   true ->
				Acc+string:str(P, string:to_upper([C]))
			end
		end, 0, String).
			

decryptxor(Message) ->
    M = h2s(Message),
    P = lists:map(fun(X) ->
		      T = lists:map(fun(P) ->
					    P bxor X
				    end, M),
		      {score(T),io_lib:format("~s",[T])} 
		  end, lists:seq(0,255)),
    Q = lists:keysort(1,P),
    {_,[Decoded]}=lists:nth(1,lists:reverse(Q)),
    Decoded.
    
hex2b64(From) ->
    T = binary:encode_unsigned(list_to_integer(From, 16)),
    base64:encode_to_string(T).

hex2b64_test() ->
    X=hex2b64("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"),
    "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" = X.

