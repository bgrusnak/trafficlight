-module(answer).
-include("trafficlight.hrl").

-compile(export_all).

-spec response(any()) -> binary().
response(Data) ->
	jsx:encode([{<<"status">>,<<"ok">>},{<<"response">>, Data}])
.

encode_error(Message) ->
	jsx:encode([{<<"status">>,<<"error">>},{<<"msg">>, Message}])
.


no_sequence() ->
	encode_error(<<"The sequence isn't found">>)
.

bad_data() ->
	encode_error(<<"Bad data">>)
.

no_solutions_found() ->
	encode_error(<<"No solutions found">>)
.


no_enough_data() ->
	encode_error(<<"There isn't enough data">>)
.

red_must_be_last() ->
	encode_error(<<"The red observation should be the last">>)
.
