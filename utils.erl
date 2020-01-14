-module(utils).
-export([hex2b64/1,tc1/0]).

hex2b64(From) ->
    T = binary:encode_unsigned(list_to_integer(From, 16)),
    base64:encode_to_string(T).

tc1() ->
    X=hex2b64("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"),
    {X == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t", X}.
