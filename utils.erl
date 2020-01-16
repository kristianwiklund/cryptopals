-module(utils).
-include_lib("eunit/include/eunit.hrl").
-export([hex2b64/1,fxor/2,decryptxor/1,readfile/1, repxor/2, hammingdist/2]).

rf(File) ->
    case file:read_line(File) of
        {ok, Data} -> [Data | rf(File)];
        eof        -> []
    end.

readfile(Filename) ->
    {ok, File} = file:open(Filename, read),
    rf(File).

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
				-1000; % there cannot be any zero in a string
			   C > 127 -> % not ascii
				-1000;
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
    {Score,[Decoded]}=lists:nth(1,lists:reverse(Q)),
    {Score,Decoded}.
    
hex2b64(From) ->
    T = binary:encode_unsigned(list_to_integer(From, 16)),
    base64:encode_to_string(T).

hex2b64_test() ->
    X=hex2b64("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"),
    "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" = X.

repxor([M|Essage], [K|Ey], Key)->
%    io:fwrite("~s ^ ~s ~B\n",[[M],[K],M bxor K]),
    [io_lib:format("~2.16.0B",[M bxor K]) | repxor(Essage, Ey, Key)];
repxor([],_,_) ->
    "";
repxor(Message,[],Key) ->
    repxor(Message,Key,Key).

repxor(Message, Key) ->
    T = repxor(Message, Key, Key),
    lists:flatten(T).
    
    

repxor_test() ->
    P = [$0|integer_to_list(list_to_integer("0B3637272A2B2E63622C2E69692A23693A2A3C6324202D623D63343C2A26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f" ,16),16)],
    X = repxor("Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal", "ICE"),
    P=X.

a2int([S|Tring],Acc) ->
    a2int(Tring,256*Acc+S);
a2int([],Acc) ->
    Acc.
a2int(String)->
    a2int(String,0).

a2int_test()->
    0 = a2int(""),
    32 = a2int(" "),
    (48*256+49) = a2int("01").

hammingdist(AS, BS) ->
    A = a2int(AS),
    B = a2int(BS),
    ABin = integer_to_list(A,2),
    BBin = integer_to_list(B,2),
    T = lists:zipwith(fun(X,Y) ->
			  X =/= Y
		  end, ABin, BBin),
    lists:foldl(fun(X, Acc) ->
			if X ->
				Acc+1;
			   true ->
				Acc
			end
		end, 0, T).

hammingdist_test() ->
    37 = hammingdist("this is a test", "wokka wokka!!!").


