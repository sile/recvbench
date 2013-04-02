-module(recv_passive).
-export([start_accept/1, start_accept/2,
         start_link/4, init/4]).

start_accept(Port) ->
    start_accept(Port, []).

start_accept(Port, Opts) ->
    ranch:start_listener(?MODULE, 100, ranch_tcp, [{port, Port}], ?MODULE, Opts).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

-record(state,
        {
          recv_length = 0 :: non_neg_integer(),
          wait        = 0 :: non_neg_integer()
        }).
init(Ref, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(Ref),
    SockOpts = proplists:get_value(sockopts, Opts, []),
    ok = inet:setopts(Socket, SockOpts),

    State = #state{
      recv_length = proplists:get_value(recv_len, Opts, 0),
      wait        = proplists:get_value(wait, Opts, 0)
     },
    loop(Socket, Transport, State).

loop(Socket, Transport, State) ->
    timer:sleep(State#state.wait),
    case Transport:recv(Socket, State#state.recv_length, infinity) of
        {ok, Data} ->
            io:format("~p: recv ~p\n", [self(), byte_size(Data)]),
            loop(Socket, Transport, State);
        Other ->
            io:format("~p: ~p\n", [self(), Other]),
            ok = Transport:close(Socket)
    end.
