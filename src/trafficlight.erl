%% Feel free to use, reuse and abuse the code in this file.

-module(trafficlight).
-include("trafficlight.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API.
-export([start/0, install/0, stop/0, get_page/2]).

%% API.

start() ->
	mnesia:create_schema([node()]),
	ok = application:start(compiler),
	ok = application:start(syntax_tools),
	ok = application:start(sync),
	ok = application:start(crypto),
	ok = application:start(cowlib),
	ok = application:start(jsx),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(goldrush),
	ok = application:start(lager),
	ok = application:start(gproc),
	ok = application:start(uuid),
	ok = application:start(cowboy_session),
	ok = application:start(mnesia),
	ok = application:start(trafficlight),
	db:start_link(),
	db:create_schema(),
,io:format("started at port 8080~n",[])	
.

stop() ->
	mnesia:stop()
.


install() ->
	db:create_schema(),
	mnesia_utile:store(#trafficlight_sequence{id= <<"2121-11121">>}),
	mnesia_utile:store(#trafficlight_event{id= <<"32327832">>, sequence= <<"2121-11121">>, numbers=[<<"0000000">>, <<"0001000">>]}),
	
ok	
.

-spec get_page(list(), list()) -> tuple().
get_page(Path, Params) ->
io:format("Get: ~p~n~p~n",[Path,Params]),
	ReqBody=jsx:encode(Params),
	ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
	{ok, Code, Headers, Body}=ibrowse:send_req(Path, ReqHeaders, post, ReqBody, [{response_format,binary}]),
	{Code, jsx:decode(Body)}
.




start_test() ->
	ibrowse:start(),
	?assertEqual({"200", [{<<"status">>, <<"ok">>}, {<<"response">>, <<"ok">>}]}, get_page("http://localhost:8080/clear", [])),
%	BadItems=[0,1,2,4,8,16,32,64],
	BadItems=[1],
	lists:map(fun(MSB) ->
		lists:map(fun(LSB) ->
			lists:map(fun(Start) -> 
				{"200", [{<<"status">>, <<"ok">>}, {<<"response">>, [{<<"sequence">>, Sequence}]}]} =  get_page("http://localhost:8080/sequence/create", []),
				lists:map(fun(Current) ->
					Mask=((MSB bsl 7)+LSB bxor 16383),
					[MB,LB]=nums:bits_to_nums(Mask band nums:bin_to_bits(nums:get_bits(Current)), 2),
					{Code, Response} = get_page("http://localhost:8080/observation/add", [{<<"observation">>, [{<<"color">>, <<"green">>}, {<<"numbers">>, [list_to_binary(MB), list_to_binary(LB)]}]}, {<<"sequence">>, Sequence}]),
io:format("Got code:~p~n~p~n", [Code, Response])
%					case length(Starts) of 
%						1 -> io:format("Found start number: ~p~nFound broken: ~p~n", [hd(Start), Missing]);
%						_ -> ok
%					end
				end, lists:seq(Start,1,-1)),
				{Code, Response} = get_page("http://localhost:8080/observation/add", [{<<"observation">>, [{<<"color">>, <<"red">>}]}, {<<"sequence">>, Sequence}]),
io:format("Got red code:~p~n~p~n", [Code, Response])
%				case length(Starts) of 
%						1 -> io:format("Found start number: ~p~nFound broken: ~p~n", [hd(Start), Missing]);
%						_ -> ok
%					end
			end, lists:seq(8, 8))
		end, BadItems)
	end, [0])
.
