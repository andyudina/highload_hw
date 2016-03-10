%% @doc: Wrapper for plain and SSL sockets. Based on
%% mochiweb_socket.erl

-module(tcp).
-export([listen/3, accept/3, recv/3, send/2, close/1, setopts/2, sendfile/5, peername/1]).

-export_type([socket/0]).

-type socket() :: {plain, inet:socket()}.

listen(plain, Port, Opts) ->
    case gen_tcp:listen(Port, Opts) of
        {ok, Socket} ->
            {ok, {plain, Socket}};
        {error, Reason} ->
            {error, Reason}
    end.



accept({plain, Socket}, Server, Timeout) ->
    case gen_tcp:accept(Socket, Timeout) of
        {ok, S} ->
            gen_server:cast(Server, accepted),
            {ok, {plain, S}};
        {error, Reason} ->
            {error, Reason}
    end.


recv({plain, Socket}, Size, Timeout) ->
    gen_tcp:recv(Socket, Size, Timeout).

send({plain, Socket}, Data) ->
    gen_tcp:send(Socket, Data).


close({plain, Socket}) ->
    gen_tcp:close(Socket).

setopts({plain, Socket}, Opts) ->
    inet:setopts(Socket, Opts).

sendfile(Fd, {plain, Socket}, Offset, Length, Opts) ->
    file:sendfile(Fd, Socket, Offset, Length, Opts).


peername({plain, Socket}) ->
    inet:peername(Socket).

