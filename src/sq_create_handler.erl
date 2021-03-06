%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(sq_create_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Code, Content} =case db:get_new_sequence() of
		undefined -> {403, answer:no_sequence()};
		ID -> 
		{200, answer:response([{sequence,list_to_binary(ID)}])}
	end,
	{ok, Req2} = cowboy_req:reply(Code, [
		{<<"content-type">>, <<"application/json">>}
	], Content, Req),
	{ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
	ok.
