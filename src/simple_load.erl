-module(simple_load).
-behaviour(gen_server).

-export([start_link/5, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_load/0, stop_load/0]). % TODO: 応答速度等の統計データが出ると良いかも

-record(state,
        {
          server_addr :: inet:ip_address() | inet:hostname(),
          server_port :: non_neg_integer(),
          process_num :: non_neg_integer(),
          byte_rate   :: non_neg_integer(),
          requests_per_sec :: non_neg_integer(),

          workers = [] :: [pid()]
        }).

start_link(ServerAddress, ServerPort, ProcessNum, ByteRate, RequestsPerSec) ->
    State = #state{
      server_addr = ServerAddress,
      server_port = ServerPort,
      process_num = ProcessNum,
      byte_rate = ByteRate,
      requests_per_sec = RequestsPerSec
     },
    gen_server:start_link({local,?MODULE}, ?MODULE, State, []).

stop() ->
    gen_server:cast(?MODULE, stop).

start_load() ->
    gen_server:cast(?MODULE, start_load).

stop_load() ->
    gen_server:cast(?MODULE, stop_load).

init(State) ->
    {ok, State}.

handle_call(Request, From, State) ->
    {stop, {unhandled_call, Request, From}, State}.

handle_cast(start_load, State) ->
    handle_start_load(State);
handle_cast(stop_load, State) ->
    handle_stop_load(State);
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info(Info, State) ->
    {stop, {unhandled_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_start_load(State) ->
    #state{server_addr=Addr, server_port=Port, process_num=Num, 
           byte_rate=ByteRate, requests_per_sec=RequestsPerSec} = State,
    Parent = self(),
    WorkerFn = fun () -> do_load(Parent, Addr, Port, ByteRate, RequestsPerSec) end,
    Workers = [spawn_link(WorkerFn) || _ <- lists:seq(1, Num)],
    {noreply, State#state{workers=Workers}}.

handle_stop_load(State) ->
    lists:foreach(fun (Worker) -> Worker ! stop end, State#state.workers),
    {noreply, State#state{workers=[]}}.

do_load(Parent, Addr, Port, ByteRate, RequestsPerSec) ->
    case gen_tcp:connect(Addr, Port, [binary, {active,false}]) of
        {error, Reason} -> 
            Parent ! {stop, self(), {error, Reason}};
        {ok, Socket} ->
            Bytes = <<0:(ByteRate*8)>>,
            do_load_loop(Parent, Socket, Bytes, RequestsPerSec)
    end.

do_load_loop(Parent, Socket, Bytes, RequestsPerSec) ->
    ok = gen_tcp:send(Socket, Bytes div RequestsPerSec),
    receive
        stop -> ok
    after (1000 div RequestsPerSec) ->
            do_load_loop(Parent, Socket, Bytes, RequestsPerSec)
    end.
