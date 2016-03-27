-module(http).
-include("server.hrl").
-include("util.hrl").


-export([start_link/2]).

-export([send_response/4]).

-export([handle_request/2, split_args/1,
         parse_path/1, keepalive_loop/3, keepalive_loop/1, accept/2]).


-spec start_link(pid(), tcp:socket()) -> pid().
start_link(Server, ListenSocket) ->
    proc_lib:spawn_link(?MODULE, accept, [Server, ListenSocket]).

-spec accept(pid(), tcp:socket()) -> ok.
accept(Server, ListenSocket) ->
    case catch tcp:accept(ListenSocket, Server, ?ACCEPT_TIMEOUT) of
        {ok, Socket} ->
            ?MODULE:keepalive_loop(Socket);
        {error, timeout} ->
            ?MODULE:accept(Server, ListenSocket);
        {error, econnaborted} ->
            ?MODULE:accept(Server, ListenSocket);
        {error, {tls_alert, _}} ->
            ?MODULE:accept(Server, ListenSocket);
        {error, closed} ->
            ok;
        {error, Other} ->
            exit({error, Other})
    end.


keepalive_loop(Socket) ->
    keepalive_loop(Socket, 0, <<>>).

keepalive_loop(Socket, NumRequests, Buffer) ->
    case ?MODULE:handle_request(Socket, Buffer) of
        {keep_alive, NewBuffer} ->
            ?MODULE:keepalive_loop(Socket, NumRequests, NewBuffer);
        {close, _} ->
            tcp:close(Socket),
            ok
    end.

-spec handle_request(tcp:socket(), binary()) ->
                            {'keep_alive' | 'close', binary()}.
handle_request(S, PrevB) ->
    {Method, RawPath, V, B0} = get_request(S, PrevB),
    {RequestHeaders, B1} = get_headers(S, V, B0),

    Req = mk_req(Method, RawPath, RequestHeaders, <<>>, V, S),
    {RequestBody, B2} = get_body(S, RequestHeaders, B1),
    Req1 = Req#req{body = RequestBody},
    Response = case is_valid_method(Req1) of
        true -> 
             execute_callback(Req1);
        false ->
             {response, 405, [], <<>>}
        end,
    Response1 = preprocess_response(Response),
    handle_response(Req1, B2, Response1).
    
preprocess_response(Response) ->
    setelement(3, Response, [{<<"Server">>, <<"hw_server">>} | element(3, Response)]).   

handle_response(Req, Buffer, {response, Code, UserHeaders, Body}) ->
    Headers = [connection(Req, UserHeaders)
               | UserHeaders],
    send_response(Req, Code, Headers, Body),
    {close_or_keepalive(Req, UserHeaders), Buffer};


handle_response(Req, Buffer, {file, ResponseCode, UserHeaders, Filename, Range}) ->
    ResponseHeaders = [connection(Req, UserHeaders) | UserHeaders],
    send_file(Req, ResponseCode, ResponseHeaders, Filename, Range),
    {close_or_keepalive(Req, UserHeaders), Buffer}.


send_response(Req, Code, Headers, UserBody) ->
    Body = case {Req#req.method, Code} of
               {'HEAD', _} -> <<>>;
               {_, 304}    -> <<>>;
               {_, 204}    -> <<>>;
               _           -> UserBody
           end,

    Response = [<<"HTTP/1.1 ">>, status(Code), <<"\r\n">>,
                encode_headers(Headers), <<"\r\n">>,
                Body],

    case tcp:send(Req#req.socket, Response) of
        ok -> ok;
        {error, Closed} when Closed =:= closed orelse Closed =:= enotconn ->
            ok
    end.


is_valid_method(Req) ->
     Req#req.method =:= 'HEAD' orelse  Req#req.method =:= 'GET'.
     
       
-spec send_file(Request::#req{}, Code::response_code(), Headers::headers(),
                Filename::file:filename(), Range::range()) -> ok.

send_file(Req, Code, Headers, Filename, {Offset, Length}) ->
    ResponseHeaders = [<<"HTTP/1.1 ">>, status(Code), <<"\r\n">>,
                       encode_headers(Headers), <<"\r\n">>],

    case file:open(Filename, [read, raw, binary]) of
        {ok, Fd} ->
            try tcp:send(Req#req.socket, ResponseHeaders) of
                ok ->
                    case tcp:sendfile(Fd, Req#req.socket, Offset, Length, []) of
                        {ok, _BytesSent} ->
                            ok;
                        {error, Closed} when Closed =:= closed orelse Closed =:= enotconn ->
                            ok
                    end;
                {error, Closed} when Closed =:= closed orelse Closed =:= enotconn ->
                    ok 
            after
                file:close(Fd)
            end;
        {error, _FileError} ->
           ok
    end, ok.

send_bad_request(Socket) ->
    Body = <<"Bad Request">>,
    Response = [<<"HTTP/1.1 ">>, status(400), <<"\r\n">>,
               <<"Content-Length: ">>, integer_to_list(size(Body)), <<"\r\n">>,
                <<"\r\n">>],
    tcp:send(Socket, Response).

execute_callback(Req) ->
    try fileserve:handle(Req) of
        {ok, Headers, {file, Filename}}       -> {file, 200, Headers, Filename, {0, 0}};
        {ok, Headers, Body}                   -> {response, 200, Headers, Body};
        {HttpCode, Headers, {file, Filename}} ->
            {file, HttpCode, Headers, Filename, {0, 0}};
        {HttpCode, Headers, Body}             -> {response, HttpCode, Headers, Body};
        {HttpCode, Body}                      -> {response, HttpCode, [], Body};
        Unexpected                            ->
            Unexpected, 
            {response, 500, [], <<"Internal server error 0">>}
    catch
        throw:{ResponseCode, Headers, Body} when is_integer(ResponseCode) ->
            {response, ResponseCode, Headers, Body};
        throw:_Exc ->
            {response, 500, [], <<"Internal server error 1">>};
        error:_Error ->
            {response, 500, [], <<"Internal server error 2">>};
        exit:_Exit ->
            {response, 500, [], <<"Internal server error 3">>}
    end.

get_request(Socket, Buffer) ->
    case erlang:decode_packet(http_bin, Buffer, []) of
        {more, _} ->
            case tcp:recv(Socket, 0, ?REQUEST_TIMEOUT) of
                {ok, Data} ->
                    NewBuffer = <<Buffer/binary, Data/binary>>,
                    get_request(Socket, NewBuffer);
                {error, timeout} ->
                    tcp:close(Socket),
                    exit(normal);
                {error, Closed} when Closed =:= closed orelse Closed =:= enotconn ->
                    tcp:close(Socket),
                    exit(normal)
            end;
        {ok, {http_request, Method, RawPath, Version}, Rest} ->
            {Method, RawPath, Version, Rest};
        {ok, {http_error, _}, _} ->
            send_bad_request(Socket),
            tcp:close(Socket),
            exit(normal);
        {ok, {http_response, _, _, _}, _} ->
            tcp:close(Socket),
            exit(normal)
    end.

-spec get_headers(tcp:socket(), version(), binary()) ->
                         {headers(), any()}.

get_headers(Socket, {1, _}, Buffer) ->
    get_headers(Socket, Buffer, [], 0).

get_headers(Socket, _, _Headers, HeadersCount)
  when HeadersCount >= ?MAX_HEADERS_NUMBER ->
    send_bad_request(Socket),
    tcp:close(Socket),
    exit(normal);
    
get_headers(Socket, Buffer, Headers, HeadersCount) ->
    case erlang:decode_packet(httph_bin, Buffer, []) of
        {ok, {http_header, _, Key, _, Value}, Rest} ->
            NewHeaders = [{ensure_binary(Key), Value} | Headers],
            get_headers(Socket, Rest, NewHeaders, HeadersCount + 1);
        {ok, http_eoh, Rest} ->
            {Headers, Rest};
        {ok, {http_error, _}, Rest} ->
            get_headers(Socket, Rest, Headers, ?HEADERS_TIMEOUT);
        {more, _} ->
            case tcp:recv(Socket, 0, ?HEADERS_TIMEOUT) of
                {ok, Data} ->
                    get_headers(Socket, <<Buffer/binary, Data/binary>>,
                                Headers, HeadersCount);
                {error, Closed} when Closed =:= closed orelse Closed =:= enotconn ->
                    tcp:close(Socket),
                    exit(normal);
                {error, timeout} ->
                    tcp:close(Socket),
                    exit(normal)
            end
    end.

-spec get_body(tcp:socket(), headers(), binary()) -> {body(), binary()}.

get_body(Socket, Headers, Buffer) ->
    case proplists:get_value(<<"Content-Length">>, Headers, undefined) of
        undefined ->
            {<<>>, Buffer};
        ContentLengthBin ->
            ContentLength = ?b2i(binary:replace(ContentLengthBin,
                                                <<" ">>, <<>>, [global])),

            ok = check_max_size(Socket, ContentLength, Buffer),

            case ContentLength - byte_size(Buffer) of
                0 ->
                    {Buffer, <<>>};
                N when N > 0 ->
                    case tcp:recv(Socket, N, ?BODY_TIMEOUT) of
                        {ok, Data} ->
                            {<<Buffer/binary, Data/binary>>, <<>>};
                        {error, Closed} when Closed =:= closed orelse Closed =:= enotconn ->
                            ok = tcp:close(Socket),
                            exit(normal);
                        {error, timeout} ->
                            ok = tcp:close(Socket),
                            exit(normal)
                    end;
                _ ->
                    <<Body:ContentLength/binary, Rest/binary>> = Buffer,
                    {Body, Rest}
            end
    end.


ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1).


check_max_size(Socket, ContentLength, Buffer) ->
    case ContentLength > ?MAX_BODY_SIZE of
        true ->
            case ContentLength < ?MAX_BODY_SIZE * 2 of
                true ->
                    OnSocket = ContentLength - size(Buffer),
                    tcp:recv(Socket, OnSocket, ?REQUEST_TIMEOUT),
                    Response = [<<"HTTP/1.1 ">>, status(413), <<"\r\n">>,
                                <<"Content-Length: 0">>, <<"\r\n\r\n">>],
                    tcp:send(Socket, Response),
                    tcp:close(Socket);
                false ->
                    tcp:close(Socket)
            end,

            exit(normal);
        false ->
            ok
    end.

-spec mk_req(Method::http_method(), {PathType::atom(), RawPath::binary()},
             RequestHeaders::headers(), RequestBody::body(), V::version(),
             Socket::tcp:socket() | undefined) ->
             #req{}.
mk_req(Method, RawPath, RequestHeaders, RequestBody, V, Socket) ->
    case parse_path(RawPath) of
        {ok, {Path, URL, URLArgs}} ->
            #req{method = Method, path = URL, args = URLArgs, version = V,
                 raw_path = Path, headers = RequestHeaders,
                 body = RequestBody, pid = self(), socket = Socket};
        {error, _Reason} ->
            send_bad_request(Socket),
            tcp:close(Socket),
            exit(normal)
    end.

encode_headers([]) ->
    [];

encode_headers([[] | H]) ->
    encode_headers(H);
encode_headers([{K, V} | H]) ->
    [encode_value(K), <<": ">>, encode_value(V), <<"\r\n">>, encode_headers(H)].


encode_value(V) when is_integer(V) -> ?i2l(V);
encode_value(V) when is_binary(V)  -> V;
encode_value(V) when is_list(V) -> list_to_binary(V).


connection_token(#req{version = {1, 1}, headers = Headers}) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"close">> -> <<"close">>;
        <<"Close">> -> <<"close">>;
        _           -> <<"Keep-Alive">>
    end;
connection_token(#req{version = {1, 0}, headers = Headers}) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"Keep-Alive">> -> <<"Keep-Alive">>;
        _                -> <<"close">>
    end;
connection_token(#req{version = {0, 9}}) ->
    <<"close">>.


close_or_keepalive(_, UserHeaders) ->
    case proplists:get_value(<<"Connection">>, UserHeaders) of
        <<"Keep-Alive">> -> keep_alive;
        _                -> close
    end.


connection(Req, UserHeaders) ->
    case proplists:get_value(<<"Connection">>, UserHeaders) of
        undefined ->
            {<<"Connection">>, connection_token(Req)};
        _ ->
            []
    end.

parse_path({abs_path, FullPath}) ->
    case binary:split(FullPath, [<<"?">>]) of
        [URL]       -> {ok, {URL, split_path(URL), []}};
        [URL, Args] -> {ok, {URL, split_path(URL), split_args(Args)}}
    end;
parse_path({absoluteURI, _Scheme, _Host, _Port, Path}) ->
    parse_path({abs_path, Path});
parse_path(_) ->
    {error, unsupported_uri}.

split_path(Path) ->
    [P || P <- binary:split(Path, [<<"/">>], [global]),
          P =/= <<>>].

-spec split_args(binary()) -> list({binary(), binary() | true}).
split_args(<<>>) ->
    [];
split_args(Qs) ->
    Tokens = binary:split(Qs, <<"&">>, [global, trim]),
    [case binary:split(Token, <<"=">>) of
        [Token] -> {Token, true};
        [Name, Value] -> {Name, Value}
    end || Token <- Tokens].

status(100) -> <<"100 Continue">>;
status(101) -> <<"101 Switching Protocols">>;
status(102) -> <<"102 Processing">>;
status(200) -> <<"200 OK">>;
status(201) -> <<"201 Created">>;
status(202) -> <<"202 Accepted">>;
status(203) -> <<"203 Non-Authoritative Information">>;
status(204) -> <<"204 No Content">>;
status(205) -> <<"205 Reset Content">>;
status(206) -> <<"206 Partial Content">>;
status(207) -> <<"207 Multi-Status">>;
status(226) -> <<"226 IM Used">>;
status(300) -> <<"300 Multiple Choices">>;
status(301) -> <<"301 Moved Permanently">>;
status(302) -> <<"302 Found">>;
status(303) -> <<"303 See Other">>;
status(304) -> <<"304 Not Modified">>;
status(305) -> <<"305 Use Proxy">>;
status(306) -> <<"306 Switch Proxy">>;
status(307) -> <<"307 Temporary Redirect">>;
status(400) -> <<"400 Bad Request">>;
status(401) -> <<"401 Unauthorized">>;
status(402) -> <<"402 Payment Required">>;
status(403) -> <<"403 Forbidden">>;
status(404) -> <<"404 Not Found">>;
status(405) -> <<"405 Method Not Allowed">>;
status(406) -> <<"406 Not Acceptable">>;
status(407) -> <<"407 Proxy Authentication Required">>;
status(408) -> <<"408 Request Timeout">>;
status(409) -> <<"409 Conflict">>;
status(410) -> <<"410 Gone">>;
status(411) -> <<"411 Length Required">>;
status(412) -> <<"412 Precondition Failed">>;
status(413) -> <<"413 Request Entity Too Large">>;
status(414) -> <<"414 Request-URI Too Long">>;
status(415) -> <<"415 Unsupported Media Type">>;
status(416) -> <<"416 Requested Range Not Satisfiable">>;
status(417) -> <<"417 Expectation Failed">>;
status(418) -> <<"418 I'm a teapot">>;
status(422) -> <<"422 Unprocessable Entity">>;
status(423) -> <<"423 Locked">>;
status(424) -> <<"424 Failed Dependency">>;
status(425) -> <<"425 Unordered Collection">>;
status(426) -> <<"426 Upgrade Required">>;
status(428) -> <<"428 Precondition Required">>;
status(429) -> <<"429 Too Many Requests">>;
status(431) -> <<"431 Request Header Fields Too Large">>;
status(500) -> <<"500 Internal Server Error">>;
status(501) -> <<"501 Not Implemented">>;
status(502) -> <<"502 Bad Gateway">>;
status(503) -> <<"503 Service Unavailable">>;
status(504) -> <<"504 Gateway Timeout">>;
status(505) -> <<"505 HTTP Version Not Supported">>;
status(506) -> <<"506 Variant Also Negotiates">>;
status(507) -> <<"507 Insufficient Storage">>;
status(510) -> <<"510 Not Extended">>;
status(511) -> <<"511 Network Authentication Required">>;
status(B) when is_binary(B) -> B.
