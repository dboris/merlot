%% merlot_supervisor - Supervisor implementation for Merlot
%%
%% Provides process supervision with restart strategies.
%% Monitors child processes and restarts them according to the
%% configured strategy when they crash.

-module(merlot_supervisor).
-export([start/2, start_link/2, stop/1, which_children/1,
         worker/2, temporary_worker/2, transient_worker/2]).

%% Create a permanent worker child spec (always restart)
worker(Id, StartFun) ->
    {Id, StartFun, 'Permanent'}.

%% Create a temporary worker (never restart)
temporary_worker(Id, StartFun) ->
    {Id, StartFun, 'Temporary'}.

%% Create a transient worker (restart only on crash)
transient_worker(Id, StartFun) ->
    {Id, StartFun, 'Transient'}.

%% Start a supervisor with a strategy and list of child specs.
%% Strategy: one_for_one | one_for_all | rest_for_one
%% ChildSpecs: [{Id, StartFun, RestartType}, ...]
%%   RestartType: permanent | temporary | transient
start(Strategy, ChildSpecs) ->
    spawn(fun() -> init(Strategy, ChildSpecs) end).

%% Start a supervisor and link it to the calling process.
start_link(Strategy, ChildSpecs) ->
    Pid = start(Strategy, ChildSpecs),
    link(Pid),
    Pid.

%% Stop the supervisor and all children.
stop(Pid) ->
    Pid ! stop,
    ok.

%% Get list of {Id, Pid} for all children.
which_children(Pid) ->
    Ref = make_ref(),
    Pid ! {which_children, self(), Ref},
    receive
        {Ref, Children} -> Children
    after 5000 ->
        []
    end.

%% Internal: initialize supervisor
init(Strategy, ChildSpecs) ->
    process_flag(trap_exit, true),
    Children = start_children(ChildSpecs, []),
    loop(Strategy, Children).

%% Internal: start all children
start_children([], Acc) ->
    lists:reverse(Acc);
start_children([{Id, StartFun, RestartType} | Rest], Acc) ->
    %% StartFun is a nullary function (takes no arguments)
    Pid = StartFun(),
    link(Pid),
    Child = {Id, Pid, RestartType, StartFun},
    start_children(Rest, [Child | Acc]).

%% Internal: supervisor loop
loop(Strategy, Children) ->
    receive
        {'EXIT', Pid, Reason} ->
            NewChildren = handle_exit(Strategy, Children, Pid, Reason),
            loop(Strategy, NewChildren);
        stop ->
            stop_children(Children);
        {which_children, From, Ref} ->
            Result = [{Id, Pid} || {Id, Pid, _, _} <- Children],
            From ! {Ref, Result},
            loop(Strategy, Children);
        _ ->
            loop(Strategy, Children)
    end.

%% Internal: handle child exit
handle_exit(Strategy, Children, Pid, Reason) ->
    case find_child(Pid, Children) of
        {ok, {Id, Pid, RestartType, StartFun}} ->
            ShouldRestart = should_restart(RestartType, Reason),
            case {Strategy, ShouldRestart} of
                {one_for_one, true} -> restart_child(Id, Pid, StartFun, Children);
                {'OneForOne', true} -> restart_child(Id, Pid, StartFun, Children);
                {one_for_one, false} -> remove_child(Pid, Children);
                {'OneForOne', false} -> remove_child(Pid, Children);
                {one_for_all, true} ->
                    stop_children(Children),
                    start_children([{I, F, R} || {I, _, R, F} <- Children], []);
                {'OneForAll', true} ->
                    stop_children(Children),
                    start_children([{I, F, R} || {I, _, R, F} <- Children], []);
                {one_for_all, false} -> remove_child(Pid, Children);
                {'OneForAll', false} -> remove_child(Pid, Children);
                {rest_for_one, true} -> restart_rest(Pid, Children);
                {'RestForOne', true} -> restart_rest(Pid, Children);
                {rest_for_one, false} -> remove_child(Pid, Children);
                {'RestForOne', false} -> remove_child(Pid, Children)
            end;
        not_found ->
            Children
    end.

%% Internal: find child by pid
find_child(_Pid, []) ->
    not_found;
find_child(Pid, [{Id, Pid, RestartType, StartFun} | _]) ->
    {ok, {Id, Pid, RestartType, StartFun}};
find_child(Pid, [_ | Rest]) ->
    find_child(Pid, Rest).

%% Internal: check if child should be restarted
should_restart(permanent, _Reason) -> true;
should_restart('Permanent', _Reason) -> true;
should_restart(temporary, _Reason) -> false;
should_restart('Temporary', _Reason) -> false;
should_restart(transient, normal) -> false;
should_restart(transient, shutdown) -> false;
should_restart(transient, _Reason) -> true;
should_restart('Transient', normal) -> false;
should_restart('Transient', shutdown) -> false;
should_restart('Transient', _Reason) -> true.

%% Internal: restart a single child
restart_child(Id, OldPid, StartFun, Children) ->
    NewPid = StartFun(),
    link(NewPid),
    replace_child(OldPid, {Id, NewPid, get_restart_type(OldPid, Children), StartFun}, Children).

%% Internal: get restart type for a child
get_restart_type(Pid, Children) ->
    case find_child(Pid, Children) of
        {ok, {_, _, RestartType, _}} -> RestartType;
        not_found -> permanent
    end.

%% Internal: replace child in list
replace_child(OldPid, NewChild, Children) ->
    [{Id, Pid, R, F} || {Id, Pid, R, F} <- Children, Pid =/= OldPid] ++ [NewChild].

%% Internal: remove child from list
remove_child(Pid, Children) ->
    [{Id, P, R, F} || {Id, P, R, F} <- Children, P =/= Pid].

%% Internal: restart crashed child and all after it
restart_rest(Pid, Children) ->
    {Before, After} = split_at_pid(Pid, Children, []),
    stop_children(After),
    NewAfter = start_children([{I, F, R} || {I, _, R, F} <- After], []),
    Before ++ NewAfter.

%% Internal: split children list at pid
split_at_pid(_Pid, [], Acc) ->
    {lists:reverse(Acc), []};
split_at_pid(Pid, [{_, Pid, _, _} = Child | Rest], Acc) ->
    {lists:reverse(Acc), [Child | Rest]};
split_at_pid(Pid, [Child | Rest], Acc) ->
    split_at_pid(Pid, Rest, [Child | Acc]).

%% Internal: stop all children
stop_children([]) ->
    ok;
stop_children([{_Id, Pid, _, _} | Rest]) ->
    unlink(Pid),
    exit(Pid, shutdown),
    stop_children(Rest).
