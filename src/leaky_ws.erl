-module(leaky_ws).

-behavior(cowboy_websocket).

-export([init/2, terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([get_wss/0, send_heap_bins/0, send_heap_bins/1, process_infos/0, gcs/0]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

terminate(Reason, _Req, _State) ->
	lager:warning("WS terminated: ~p", [Reason]).
	
websocket_init(State) ->
	{[{set_options, #{idle_timeout => infinity}}], State}.

websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({reply, _Reply, _SessionInfo}, State) ->
    lager:warning("xz"),
	% lager:log(warning, [], "xz"),
	% io:format("xz"),
    {ok, State};
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
	lists:foreach(fun(Pid) ->
		[Pid ! {reply, crypto:strong_rand_bytes(64),  #{}} || _ <- lists:seq(1, N)] end,
		WSs).

process_infos() ->
	WSs = get_wss(),
	lists:map(fun(Pid) -> 
		{Pid, process_info(Pid, 
			[memory, stack_size,  total_heap_size, heap_size, message_queue_len, reductions, garbage_collection])} end, WSs).

gcs() ->
	WSs = get_wss(),
	lists:foreach(fun(Pid) ->
		erlang:garbage_collect(Pid) end, WSs).
