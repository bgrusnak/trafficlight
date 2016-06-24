%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(ob_add_handler).
-include_lib("eunit/include/eunit.hrl").
-include("trafficlight.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([process_color/3]).
-export([get_missed/2]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Body, Req1} = cowboy_req:body(Req),  
	{Code, Content} =case try jsx:decode(Body, [return_maps]) catch _:_ -> undefined end of
		undefined ->{403, answer:no_sequence()};
		Got -> process(Got)
	end,
	{ok, Req2} = cowboy_req:reply(Code, [
		{<<"content-type">>, <<"application/json">>}
	], Content, Req),
	{ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
	ok.


process(Got) ->
	try
		Observation=maps:get( <<"observation">>, Got),
		process_observation(Got, Observation)
	catch _:_ -> {403, answer:bad_data()}
	end
.

process_observation(Got, Observation) ->
	try
		Sequence=maps:get( <<"sequence">>, Got),
		process_sequence(Sequence, Observation)
	catch _:_ ->
		{403, answer:no_sequence()}
	end
.

process_sequence(Sequence, Observation) ->
	try
		Color=maps:get( <<"color">>, Observation),
		process_color(Sequence, Observation, Color)
	catch _:_ -> {403, answer:bad_data()}
	end
.

process_color(Sequence, Observation, Color) when Color== <<"green">> ->
	Numbers=maps:get( <<"numbers">>, Observation),
	case db:is_sequence_exists(Sequence) of
		true -> 
			case db:is_already_posted(Sequence, Numbers) of 
				true -> {403, answer:no_solutions_found()};
				_ -> case db:get_color(Sequence) of
					<<"red">> -> {403, answer:red_must_be_last()};
					_ -> process_numbers(Sequence, Numbers)
				end
			end;
		_ -> {403, answer:no_sequence()}
	end
;

process_color(Sequence, Numbers, Color) when Color== <<"red">> ->
	case db:is_sequence_empty(Sequence) of
		true -> 
			{403, answer:no_enough_data()};
		_ ->
			db:set_color(Sequence, Color),
			do_red(Sequence)
	end
;

process_color(Sequence, Numbers, Color) ->
	{403, answer:bad_data()}
.

process_numbers(Sequence, Numbers) when is_list(Numbers)->
	try
		[N1, N2]=Numbers,
		case ({re:run(N1, "[01]{7}"), re:run(N2, "[01]{7}")}) of
			{nomatch, _} -> {403, answer:bad_data()};
			{_, nomatch} -> {403, answer:bad_data()};
			_ ->
				process_all(Sequence, Numbers)
		end
	catch _:_ ->
		{403, answer:bad_data()}
	end
.


process_all(Sequence, Numbers) ->
	db:set_color(Sequence, <<"green">>),
	[N1, N2]=Numbers,
	N=list_to_binary(lists:append(binary_to_list(N1), binary_to_list(N2))),
	db:add_event(Sequence, N),
	do_green(Sequence).

do_green(Sequence) ->
	Events=db:get_events(Sequence),
	EL=length(Events),
	Parse=lists:foldl(fun(X, A) -> 
		case A#dolist.found of
			false -> 
				Variants=nums:get_variants(X#trafficlight_event.numbers, decrease(A#dolist.variants)),
				case length (Variants) of
					0 -> #dolist{error=true, found=true};
					1 -> 
						Num=hd(Variants),
						Started=Num+A#dolist.passed,
						#dolist{found=true, started=[Started]};
					_ ->
						Started=lists:map(fun(X) -> X+A#dolist.passed end, Variants),
						#dolist{variants=Variants, passed=A#dolist.passed+1, started=Started}
				end
			;
			_ ->
				A
		end
	end, #dolist{variants=lists:seq(2, 100)}, Events),
	
	case Parse#dolist.error of
		true -> {403, answer:no_solutions_found()};
		_ ->
			Missings=get_missed(lists:max(Parse#dolist.started),Events),
			Miss=nums:bits_to_nums(Missings, 2),
			{200, answer:response([{<<"start">>, Parse#dolist.started}, {<<"missing">>, Miss}])}
	end	

.

do_red(Sequence) ->
	Events=lists:reverse(db:get_events(Sequence)),
	LE=length(Events),
	Missings=get_missed(LE,Events),
	Miss=nums:bits_to_nums(Missings, 2),
	{200, answer:response([{start, [LE]}, {missing, Miss}])}
	
.
	


get_missed(Number, Events) ->
	{_, Missed, _} = lists:foldl(fun(Event, {Started, Missed, Count}) -> 
		NB=nums:get_broken((Started-Count), Event#trafficlight_event.numbers, 2) bor Missed,
		{Started, NB, Count+1} end, {Number, 0, 0}, Events),
	Missed
.

decrease(List) ->
	lists:reverse(lists:foldl(fun(X, A) -> case (X < 2) of  true -> A; _ -> [X-1|A] end end, [], List))
.
	

% --------------------------------- tests ---------------------------s	

get_missed_test() ->
	Events=[#trafficlight_event{numbers= <<"00000001111110">>}, #trafficlight_event{numbers= <<"00000001010010">>}, #trafficlight_event{numbers= <<"00000001101110">>}, #trafficlight_event{numbers= <<"00000001101010">>}, #trafficlight_event{numbers= <<"00000000111010">>}, #trafficlight_event{numbers= <<"00000001011010">>}, #trafficlight_event{numbers= <<"00000001011100">>}, #trafficlight_event{numbers= <<"00000000010010">>}],
	?assertEqual(15233, get_missed(8,Events))

.	
	
	
	
	
decrease_test ()->
	?assertEqual([4,7,8], decrease([5,8,9])),
	?assertEqual([1,3], decrease([1,2,4]))
.	
