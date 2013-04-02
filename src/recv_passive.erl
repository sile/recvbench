-module(recv_passive).
-export([start_accept/1, start_link/4, init/4]).

start_accept(Port) ->
    ranch:start_listener(?MODULE, 100, ranch_tcp, [{port, Port}], ?MODULE,[]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            io:format("~p: recv ~p\n", [self(), byte_size(Data)]),
            loop(Socket, Transport);
        Other ->
            io:format("~p: ~p\n", [self(), Other]),
            ok = Transport:close(Socket)
    end.
