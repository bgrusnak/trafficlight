-module(db).
-include("trafficlight.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([get_new_sequence/0, is_sequence_exists/1, is_sequence_empty/1, 
is_already_posted/2, add_event/2, get_events/0, get_events/1, set_color/2, get_color/1, create_schema/0, clear_all/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
     gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
    {ok, []}.

get_new_sequence() ->
  gen_server:call({global, ?MODULE}, {get_new_sequence}).
  
is_sequence_exists(Sequence) ->
  gen_server:call({global, ?MODULE}, {is_sequence_exists, Sequence}).
  
is_sequence_empty(Sequence) ->
  gen_server:call({global, ?MODULE}, {is_sequence_empty, Sequence}).
  
is_already_posted(Sequence, Numbers) ->
  gen_server:call({global, ?MODULE}, {is_already_posted, Sequence, Numbers}).
  
add_event(Sequence, Numbers) ->
  gen_server:call({global, ?MODULE}, {add_event, Sequence, Numbers}).
  
get_events() ->
  gen_server:call({global, ?MODULE}, {is_already_posted}).
  
get_events(Sequence) ->
  gen_server:call({global, ?MODULE}, {get_events, Sequence}).
  
set_color(Sequence, Color) ->
  gen_server:call({global, ?MODULE}, {set_color, Sequence, Color}).
  
get_color(Sequence) ->
  gen_server:call({global, ?MODULE}, {get_color, Sequence}).
  
create_schema() ->
  gen_server:call({global, ?MODULE}, {create_schema}).
  
clear_all() ->
  gen_server:call({global, ?MODULE}, {clear_all}).

handle_call({ get_new_sequence }, _From, State) ->
	Reply=try
		ID=uuid:v4(),
		TS=erlang:timestamp(),
		mnesia_utile:store(#trafficlight_sequence{id= ID, createdate=TS}),
		uuid:to_string(ID)
	catch _:_ ->
		undefined
	end,
	{ reply, Reply, State };


handle_call({ is_sequence_exists, Sequence }, _From, State) ->
	Reply=try
		case mnesia_utile:find_by_id(trafficlight_sequence, 
		uuid:to_binary(binary_to_list(Sequence))) of
			no_rows -> false;
			not_found -> false;
			_ -> true
		end
	catch _:_ ->
		true
	end,
	{ reply, Reply, State };
	

handle_call({ is_sequence_empty, Sequence }, _From, State) ->
	BS=uuid:to_binary(binary_to_list(Sequence)),
	Reply=case mnesia_utile:find(trafficlight_event,fun (R) -> BS==R#trafficlight_event.sequence end) of
		no_rows -> true;
		not_found -> true;
		[] -> true;
		_ -> false
	end,
	{ reply, Reply, State };

handle_call({ is_already_posted, Sequence, Numbers }, _From, State) ->
	BS=uuid:to_binary(binary_to_list(Sequence)),
	Reply=case mnesia_utile:find(trafficlight_event,fun (R) -> (BS==R#trafficlight_event.sequence) and (Numbers==R#trafficlight_event.numbers) end) of
		no_rows -> false;
		not_found -> false;
		[] -> false;
		_ -> true
	end,
	{ reply, Reply, State };

handle_call({ add_event, Sequence, Numbers }, _From, State) ->
	Id=uuid:v4(),
	BS=uuid:to_binary(binary_to_list(Sequence)),
	mnesia_utile:store(#trafficlight_event{id= Id, sequence=BS, 
	numbers=Numbers, createdate=erlang:timestamp()}),
	{ reply, ok, State };

handle_call({get_events }, _From, State) ->
	E=case mnesia_utile:all(trafficlight_event) of
		no_rows -> [];
		not_found -> [];
		RT -> RT
	end,
	Reply=lists:sort(fun(A,B) ->
		(B#trafficlight_event.createdate > A#trafficlight_event.createdate)
	end, E),
	{ reply, Reply, State };

handle_call({get_events, Sequence }, _From, State) ->
	BS=uuid:to_binary(binary_to_list(Sequence)),
	E=case mnesia_utile:find(trafficlight_event,fun (R) -> BS==R#trafficlight_event.sequence end) of
		no_rows -> [];
		not_found -> [];
		RT -> RT
	end,
	Reply=lists:sort(fun(A,B) ->
		(B#trafficlight_event.createdate > A#trafficlight_event.createdate)
	end, E),
	{ reply, Reply, State };

handle_call({set_color, Sequence, Color }, _From, State) ->
	BS=uuid:to_binary(binary_to_list(Sequence)),
	Id=case mnesia_utile:find(trafficlight_state,fun (R) -> BS==R#trafficlight_state.sequence end) of
		no_rows -> uuid:v4();
		not_found -> uuid:v4();
		[] -> uuid:v4();
		Fnd -> 
			Rec=hd(Fnd),
			Rec#trafficlight_state.id
	end,
	mnesia_utile:store(#trafficlight_state{id= Id, sequence=BS, color=Color}),
	{ reply, ok, State };
	
handle_call({get_color, Sequence }, _From, State) ->
	BS=uuid:to_binary(binary_to_list(Sequence)),
	Reply=case mnesia_utile:find(trafficlight_state,fun (R) -> BS==R#trafficlight_state.sequence end) of
		no_rows -> undefined;
		not_found -> undefined;
		[] -> undefined;
		Fnd -> 
			Rec=hd(Fnd),
			Rec#trafficlight_state.color
	end,
	{ reply, Reply, State };

handle_call({create_schema }, _From, State) ->
	mnesia:create_table(trafficlight_sequence, [{attributes, record_info(fields, trafficlight_sequence)},{disc_copies,[node()]}]),
	mnesia:create_table(trafficlight_broken, [{attributes, record_info(fields, trafficlight_broken)},{disc_copies,[node()]}]),
	mnesia:create_table(trafficlight_event, [{attributes, record_info(fields, trafficlight_event)},{disc_copies,[node()]}, {index, [#trafficlight_event.sequence, #trafficlight_event.numbers]}]),
	mnesia:create_table(trafficlight_state, [{attributes, record_info(fields, trafficlight_state)},{disc_copies,[node()]}, {index, [#trafficlight_state.sequence]}]),
	{ reply, ok, State };

handle_call({clear_all }, _From, State) ->
	mnesia:clear_table(trafficlight_sequence),
	mnesia:clear_table(trafficlight_broken),
	mnesia:clear_table(trafficlight_event),
	mnesia:clear_table(trafficlight_state),
	{ reply, ok, State }
.

handle_cast(_Message, State) -> { noreply, State }.
handle_info(_Message, State) -> { noreply, State }.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> { ok, State }.
