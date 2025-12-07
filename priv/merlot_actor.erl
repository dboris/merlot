%% merlot_actor - Actor implementation for Merlot
%%
%% Provides a simple actor (stateful process) abstraction.
%% Actors handle messages via a handler function that returns
%% either {continue, NewState} or {stop, FinalState}.

-module(merlot_actor).
-export([start/2, start_link/2, send/2]).

%% Start an actor with initial state and handler function.
%% Handler is called as Handler(State, Message) and should return:
%%   {continue, NewState} - continue with new state
%%   {stop, FinalState} - terminate the actor
start(InitState, Handler) ->
    spawn(fun() -> loop(InitState, Handler) end).

%% Start an actor and link it to the calling process.
start_link(InitState, Handler) ->
    Pid = start(InitState, Handler),
    link(Pid),
    Pid.

%% Send a message to an actor (async).
send(Pid, Msg) ->
    Pid ! Msg,
    ok.

%% Internal: actor message loop
%% Handler can be:
%%   - A 2-arity function: Handler(State, Msg)
%%   - A curried function: (Handler(State))(Msg)
loop(State, Handler) ->
    receive
        Msg ->
            Result = call_handler(Handler, State, Msg),
            case Result of
                {continue, NewState} ->
                    loop(NewState, Handler);
                {'Continue', NewState} ->
                    %% Handle OCaml poly variant format
                    loop(NewState, Handler);
                {stop, _FinalState} ->
                    ok;
                {'Stop', _FinalState} ->
                    %% Handle OCaml poly variant format
                    ok
            end
    end.

%% Internal: call handler, detecting arity
call_handler(Handler, State, Msg) ->
    case erlang:fun_info(Handler, arity) of
        {arity, 2} ->
            %% Direct 2-arity function
            Handler(State, Msg);
        {arity, 1} ->
            %% Curried function
            (Handler(State))(Msg);
        _ ->
            %% Default to curried
            (Handler(State))(Msg)
    end.
