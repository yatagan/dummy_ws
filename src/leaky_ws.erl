-module(leaky_ws).

-behavior(cowboy_websocket).

-export([init/2, terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([get_wss/0, send_heap_bins/0, send_heap_bins/1, process_infos/0, gcs/0]).

-record(state, {session_info}).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

terminate(Reason, _Req, _State) ->
	lager:warning("WS terminated: ~p", [Reason]).
	
websocket_init(State) ->
	{[{set_options, #{idle_timeout => infinity}}], #state{}}.

websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({reply, Reply, SessionInfo}, State) ->
	Args = <<"{\"request\":\"", Reply/binary, "\",\"response\":\"", Reply/binary,"\"}">>,
    lager:info("xz ~p", [Args]),
	% lager:log(warning, [], "xz"),
	% io:format("xz"),
    {ok, State#state{session_info = SessionInfo}};
websocket_info(_Info, State) ->
	{[], State}.

get_wss() ->
	lists:filter(
		fun(Pid) -> 
			case process_info(Pid, current_function) of 
				{current_function, {cowboy_websocket, loop, 3}} -> true; 
				_ -> false 
			end 
		end, 
	processes()).

send_heap_bins() ->
	send_heap_bins(100000).
send_heap_bins(N) ->
	WSs = get_wss(),
	lists:foreach(fun(Pid) -> send_heap_binaries(Pid, N) end, WSs).

send_heap_binaries(Pid, N) ->
	Chunks = n_length_chunks(lists:seq(1, N), 10000),
	lists:foreach(fun(Chunk) ->
		ok = wait_messages_proceeded(Pid),
		lists:foreach(fun(_) ->
			Reply = crypto:strong_rand_bytes(64),
			SessionInfo = maps:from_list([{crypto:strong_rand_bytes(64), crypto:strong_rand_bytes(64)} || _ <- lists:seq(1, 100)]),
			Pid ! {reply, Reply, SessionInfo}
		end, 
		Chunk
		)
	end,
	Chunks
	).

n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
	[List];
n_length_chunks(List,Len) ->
	{Head,Tail} = lists:split(Len,List),
	[Head | n_length_chunks(Tail,Len)].

wait_messages_proceeded(Pid) ->
	#{message_queue_len := ML, total_heap_size := HS} = 
		maps:from_list(process_info(Pid, [message_queue_len, total_heap_size])),
	if ML =/= 0 -> 
		lager:error("WS info: mq len=~b, heap size=~b", [ML, HS]), 
		timer:sleep(ML), 
		wait_messages_proceeded(Pid); 
	true -> ok 
	end.

process_infos() ->
	WSs = get_wss(),
	lists:map(fun(Pid) -> 
		{Pid, process_info(Pid, 
			[memory, stack_size,  total_heap_size, heap_size, message_queue_len, reductions, garbage_collection])} end, WSs).

gcs() ->
	WSs = get_wss(),
	lists:foreach(fun(Pid) ->
		erlang:garbage_collect(Pid) end, WSs).
