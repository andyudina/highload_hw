-module(server).
-behaviour(gen_server).
-include("server.hrl").

-export([start_link/0,
         stop/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type req() :: #req{}.
-export_type([req/0, body/0, headers/0]).

-record(state, {socket :: tcp:socket(),
                acceptors :: non_neg_integer(),
                open_reqs :: non_neg_integer(),
                options :: [{_, _}]
}).

start_link() -> gen_server:start_link(?MODULE, [], []).

stop(S) ->
    gen_server:call(S, stop).


init(_Opts) ->
    process_flag(trap_exit, true),

    IPAddress = {0, 0, 0, 0},
    Port = 80,
    MinAcceptors = 20,

    {ok, Socket} = tcp:listen(plain, Port, [binary,
                                                    {ip, IPAddress},
                                                    {reuseaddr, true},
                                                    {backlog, 32768},
                                                    {packet, raw},
                                                    {active, false}
                                                   ]),

    Acceptors = ets:new(acceptors, [private, set]),
    StartAcc  = fun() ->
        Pid = http:start_link(self(), Socket), %),
        ets:insert(Acceptors, {Pid})
    end,
    [ StartAcc() || _ <- lists:seq(1, MinAcceptors)],

    {ok, #state{socket = Socket,
                acceptors = Acceptors,
                open_reqs = 0}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(accepted, State) ->
    {noreply, start_add_acceptor(State)};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', _Pid, {error, emfile}}, State) ->
    error_logger:error_msg("No more file descriptors, shutting down~n"),
    {stop, emfile, State};

handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, remove_acceptor(State, Pid)};

handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:error_msg("Elli request (pid ~p) unexpectedly "
                           "crashed:~n~p~n", [Pid, Reason]),
    {noreply, remove_acceptor(State, Pid)}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

remove_acceptor(State, Pid) ->
    ets:delete(State#state.acceptors, Pid),
    State#state{open_reqs = State#state.open_reqs - 1}.

start_add_acceptor(State) ->
    Pid = http:start_link(self(), State#state.socket),
    ets:insert(State#state.acceptors, {Pid}),
    State#state{open_reqs = State#state.open_reqs + 1}.

