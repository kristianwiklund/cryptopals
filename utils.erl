-module(utils).
-include_lib("eunit/include/eunit.hrl").
-export([h2s/1,hex2b64/1,fxor/2,decryptxor/1,decryptxor/2,readfile/1, repxor/2, hammingdist/2, findkeysize/3, chunkify/2]).
-export([shufflechunks/2]).

rf(File) ->
    case file:read_line(File) of
        {ok, Data} -> [string:trim(Data) | rf(File)];
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
			
decryptxor(Message, hex) ->
    M = h2s(Message),
    decryptxor(M).

decryptxor(M) ->
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
    [io_lib:format("~2.16.0B",[(M bxor K)]) | repxor(Essage, Ey, Key)];
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
    
    if 
	length(AS) == length(BS) ->
	    A = a2int(AS),
	    B = a2int(BS),
	    %ABin = integer_to_list(A,2),
	    %BBin = integer_to_list(B,2),
	    L = length(AS)*8,
	    ABin = lists:flatten(io_lib:format("~*.2.0B",[L,A])),
	    BBin = lists:flatten(io_lib:format("~*.2.0B",[L,B])),
%	    io:fwrite("~s ~s\n",[ABin,BBin]),
	    
	    T = lists:zipwith(fun(X,Y) ->
				      X =/= Y
			      end, ABin, BBin),
	    lists:foldl(fun(X, Acc) ->
				if X ->
					Acc+1;
				   true ->
					Acc
				end
			end, 0, T);
	true ->
	    max(length(AS),length(BS))*8
    end.


hammingdist_test() ->
    37 = hammingdist("this is a test", "wokka wokka!!!").


chunkify(Text, Size) when length(Text) >= Size ->
    {T1,T2} = lists:split(Size, Text),
    [T1|chunkify(T2,Size)];
chunkify(_,_) ->
    [].

chunkify_test() ->
    ["abcd","efgh","ijkl","mnop","qrst","uvwx"] = chunkify("abcdefghijklmnopqrstuvwxyz",4),
    ["abc","def","ghi","jkl","mno","pqr","stu","vwx"] 
	= chunkify("abcdefghijklmnopqrstuvwxyz",3).
	 
allhamming([S1,S2|Chunks],L,Acc) ->
    Dist = hammingdist(S1,S2),
    allhamming(Chunks, L, Acc+Dist);

allhamming(_,L,Acc) ->
    Acc.


findkeysize(Text, [K|Eysize]) ->
    
    Chunks = chunkify(Text, K),
    if 
	length(Chunks)>1 ->
	    Dist = allhamming(Chunks, length(Chunks), 0)/(length(Chunks)-1),
%	    io:fwrite("\"~s\" \"~s\" ~B\n", [S1,S2,Dist]),
	    [{K,Dist/K}|findkeysize(Text, Eysize)];
	true ->
	    []
    end;

findkeysize(_,[]) ->
    [].

% swap around the string and make chunks created by picking items at distance Length
shufflechunks(String, Length) ->
    lists:map(fun(Y) ->
			 lists:map(fun(X) ->
					   lists:nth(X,String)
				   end, lists:seq(Y,length(String),Length))
		 end, lists:seq(1,Length)).


shufflechunks_test() ->
    ["147","258","369"] = shufflechunks("123456789",3).    

findkeysize(Text, Low, High) ->
    lists:keysort(2,findkeysize(Text, lists:seq(Low, min(High,length(Text) div 2)))).

% based on a priori knowledge of the answer for 1:6
findkeysize_test() ->
    Rawdata = utils:readfile("data1_6.txt"),
    Text=lists:flatten(lists:map(fun(X) ->
					 base64:decode_to_string(X) 
				 end, Rawdata)),
    {D1,D2} = lists:nth(1,utils:findkeysize(Text, 2,40)),
    29 = D1. 


decrypt_running_xor_test() ->
    String = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal", 
    P=repxor(String, "ICE"),
    Q=h2s(P),
    [{Size,_}|_]=utils:findkeysize(Q,2,22),
    Chunks = utils:shufflechunks(Q,Size),
    Candidates = lists:map(fun(X)->
                      utils:decryptxor(X)
                           end, Chunks),
    FT=lists:map(fun({_,X})->X end, Candidates),
    F=lists:flatten(FT),
    String=lists:flatten(utils:shufflechunks(F,length(lists:nth(1,FT)))).
    

