-module(recv_active_once).
-export([start_accept/1, start_accept/2,
         start_link/4, init/4]).

start_accept(Port) ->
    start_accept(Port, []).

start_accept(Port, Opts) ->
    ranch:start_listener(?MODULE, 100, ranch_tcp, [{port, Port}], ?MODULE, Opts).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(Ref),

    SockOpts = proplists:get_value(sockopts, Opts, []),
    ok = inet:setopts(Socket, [{active,once}] ++ SockOpts),
    
    loop(Socket, Transport).

loop(Socket, Transport) ->
    receive
        {tcp, _, _Data} ->
            %%io:format("~p: recv ~p\n", [self(), byte_size(Data)]),
            inet:setopts(Socket, [{active,once}]),
            loop(Socket, Transport);
        Other ->
            io:format("~p: ~p\n", [self(), Other]),
            ok = Transport:close(Socket)
    end.
