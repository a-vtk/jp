-module(jp_plan).
-feature(maybe_expr, enable).

-include_lib("job_planner/include/jp_task.hrl").

-export([
    plan/1
]).

-type index() :: #{
    task_name() => task()
}.


-record(sort_state, {
    visited = #{} :: #{task_name() => started | visited},
    trace = [] :: [task_name()],
    ordering = [] :: [task()]
}).
-type sort_state() :: #sort_state{}.


-type err_reason()
    :: {duplicate, task_name()}
     | {cycle, [task_name()]}
     | {dependency_not_found, task_name()}.


-export_type([
    err_reason/0
]).

%% API

-spec plan([task()]) -> {ok, [task()]} | {error, err_reason()}.

plan(Tasks) ->
    maybe
        {ok, Index} ?= create_index(Tasks),
        topological_sort(Index)
    end.

%% internal

-spec create_index([task()]) -> {ok, index()} | {error, term()}.

create_index(Tasks) ->
    jp_utils:maybe_foldl(fun add_to_index/2, #{}, Tasks).


-spec add_to_index(task(), index()) -> {ok, index()} | {error, {duplicate, task_name()}}.

add_to_index(#task{name = TaskName} = Task, Index) ->
    case maps:find(TaskName, Index) of
        error ->
            {ok, maps:put(TaskName, Task, Index)};

        {ok, _} ->
            {error, {duplicate, TaskName}}
    end.


-spec topological_sort(index()) -> {ok, [task()]} | {error, term()}.

topological_sort(Index) ->
    State = #sort_state{},
    Iter = maps:iterator(Index, ordered),
    topological_sort(maps:next(Iter), Index, State).


-spec topological_sort(Iterator, Index, State) -> {ok, Tasks} | {error, Reason} when
      Iterator :: {Key, Value, maps:iterator()} | none,
      Key :: task_name(),
      Value :: task(),
      Index :: index(),
      State :: sort_state(),
      Tasks :: [task()],
      Reason :: term().

topological_sort(none, _Index, #sort_state{ordering = Ordering}) ->
    Ordering1 = lists:reverse(lists:flatten(Ordering)),
    {ok, Ordering1};

topological_sort({_TaskName, Task, Iter}, Index, State) ->
    case traverse_task(Task, Index, State) of
        {ok, State1} ->
            topological_sort(maps:next(Iter), Index, State1);

        {error, _} = Error ->
            Error
    end.


-spec traverse_task(Task, Index, State1) -> {ok, State2} | {error, Reason} when
      Task :: task(),
      Index :: index(),
      State1 :: State2,
      State2 :: sort_state(),
      Reason :: term().

traverse_task(Task, Index, State) ->
    #sort_state{
        visited = Visited,
        trace = Trace
    } = State,

    #task{
        name = TaskName,
        deps = Deps
    } = Task,

    case maps:find(TaskName, Visited) of
        {ok, started} ->
            Trace1 = lists:reverse([TaskName | Trace]),
            Cycle = extract_cycle_from_path(Trace1, TaskName),
            {error, {cycle, Cycle}};

        {ok, visited} ->
            {ok, State};

        error ->
            Visited1 = maps:put(TaskName, started, Visited),
            State1 = State#sort_state{
                visited = Visited1,
                trace = [TaskName | Trace]
            },

            case traverse_deps(Deps, Index, State1) of
                {ok, State2} ->
                    #sort_state{
                        visited = Visited2,
                        ordering = Ordering
                    } = State2,

                    {ok, State2#sort_state{
                        visited = maps:put(TaskName, visited, Visited2),
                        trace = [],
                        ordering = [Task | Ordering]
                    }};

                {error, _} = Error ->
                    Error
            end
    end.


-spec traverse_deps([TaskName], Index, State1) -> {ok, State2} | {error, Reason} when
      TaskName :: task_name(),
      Index :: index(),
      State1 :: State2,
      State2 :: sort_state(),
      Reason :: term().

traverse_deps([], _Index, State) ->
    {ok, State};
traverse_deps([TaskName | Deps], Index, State) ->
    maybe
        {ok, Task} ?= find_dependency(TaskName, Index),
        {ok, State1} ?= traverse_task(Task, Index, State),
        traverse_deps(Deps, Index, State1)
    end.


-spec find_dependency(task_name(), index()) -> {ok, task()} | {error, term()}.

find_dependency(TaskName, Index) ->
    case maps:find(TaskName, Index) of
        {ok, Task} ->
            {ok, Task};

        error ->
            {error, {dependency_not_found, TaskName}}
    end.


-spec extract_cycle_from_path([task_name()], task_name()) -> [task_name()].

extract_cycle_from_path(Path, LastTaskName) ->
    lists:dropwhile(fun (TaskName) -> TaskName =/= LastTaskName end, Path).
