-module(recv_active_n).
-export([start_accept/2, start_accept/3,
         start_link/4, init/4]).

start_accept(Port, N) ->
    start_accept(Port, [{active_n, N}]).

start_accept(Port, N, Opts) ->
    ranch:start_listener(?MODULE, 100, ranch_tcp, [{port, Port}], ?MODULE, [{active_n, N} | Opts]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(Ref),

    SockOpts = proplists:get_value(sockopts, Opts, []),
    N = proplists:get_value(active_n, Opts),
    ok = inet:setopts(Socket, [{active,N}] ++ SockOpts),

    loop(Socket, N, Transport).

loop(Socket, N, Transport) ->
    receive
        {tcp, _, _Data} ->
            loop(Socket, N, Transport);
        {tcp_passive, _} ->
            inet:setopts(Socket, [{active,N}]),
            loop(Socket, N, Transport);
        Other ->
            io:format("~p: ~p\n", [self(), Other]),
            ok = Transport:close(Socket)
    end.
