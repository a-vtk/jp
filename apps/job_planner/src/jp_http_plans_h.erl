-module(jp_http_plans_h).
-feature(maybe_expr, enable).

-include_lib("job_planner/include/jp_task.hrl").

-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    from_json/2,
    to_json/2,
    to_shell/2
]).

%% cowboy_rest callbacks

-spec init(Req, State) -> {cowboy_rest, Req, State} when
      Req :: cowboy_req:req(),
      State :: term().

init(Req, State) ->
    {cowboy_rest, Req, State}.


-spec allowed_methods(Req, State) -> {[Method], Req, State} when
      Method :: binary(),
      Req :: cowboy_req:req(),
      State :: term().

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"PUT">>], Req, State}.


-spec content_types_provided(Req, State) -> {[Provided], Req, State} when
      Provided :: {{binary(), binary(), [term()]}, atom()},
      Req :: cowboy_req:req(),
      State :: term().

content_types_provided(Req, State) ->
    Provided = [
        {{<<"application">>, <<"json">>, []}, to_json},
        {{<<"application">>, <<"x-sh">>, []}, to_shell}
    ],
    {Provided, Req, State}.


-spec content_types_accepted(Req, State) -> {[Accepted], Req, State} when
      Accepted :: {{binary(), binary(), [term()]}, atom()},
      Req :: cowboy_req:req(),
      State :: term().

content_types_accepted(Req, State) ->
    Accepted = [
        {{<<"application">>, <<"json">>, []}, from_json}
    ],
    {Accepted, Req, State}.


-spec from_json(Req, State) -> {boolean(), Req, State} when
      Req :: cowboy_req:req(),
      State :: term().

from_json(Req, State) ->
    MediaType = get_media_type(Req),
    maybe
        {ok, Body, Req1} ?= cowboy_req:read_body(Req),
        {ok, Tasks} ?= parse(Body),
        {ok, OrderedTasks} ?= jp_plan:plan(Tasks),

        ResultBody = serialyse_tasks(OrderedTasks, #{type => MediaType}),
        Req2 = cowboy_req:set_resp_body(ResultBody, Req1),
        {true, Req2, State}
    else
        {more, _, Req3} ->
            ErrBody = serialyse_error(body_limit_exceeded, #{type => MediaType}),
            Req4 = cowboy_req:set_resp_body(ErrBody, Req3),
            {false, Req4, State};

        {error, Reason} ->
            ErrBody = serialyse_error(Reason, #{type => MediaType}),
            Req3 = cowboy_req:set_resp_body(ErrBody, Req),
            {false, Req3, State}
    end.


-spec to_json(Req, State) -> {Body :: binary(), Req, State} when
      Req :: cowboy_req:req(),
      State :: term().

to_json(Req, State) ->
    Body = serialyse_tasks([], #{type => json}),
    {Body, Req, State}.


-spec to_shell(Req, State) -> {Body :: binary(), Req, State} when
      Req :: cowboy_req:req(),
      State :: term().

to_shell(Req, State) ->
    Body = serialyse_tasks([], #{type => shell}),
    {Body, Req, State}.

%% internal


-spec get_media_type(cowboy_req:req()) -> json | shell.

get_media_type(#{media_type := {<<"application">>, <<"json">>, []}}) ->
    json;
get_media_type(#{media_type := {<<"application">>, <<"x-sh">>, []}}) ->
    shell.


-spec parse(binary() | term()) -> {ok, [task()]} | {error, invalid_json}.

parse(Body) ->
    maybe
        true ?= is_binary(Body),

        {ok, #{<<"tasks">> := Tasks}, <<>>} ?= jsone:try_decode(Body),
        true ?= is_list(Tasks),

        jp_utils:maybe_map(fun parse_task/1, Tasks)
    else
        _ ->
            {error, invalid_json}
    end.


-spec parse_task(map() | term()) -> {ok, task()} | {error, invalid_json}.

parse_task(RawTask) ->
    maybe
        true ?= is_map(RawTask),

        {ok, Name} ?= maps:find(<<"name">>, RawTask),
        true ?= is_binary(Name),

        {ok, Command} ?= maps:find(<<"command">>, RawTask),
        true ?= is_binary(Command),

        {ok, Deps} ?= parse_task_deps(RawTask),

        {ok, #task{name = Name, command = Command, deps = Deps}}
    else
        _ ->
            {error, invalid_json}
    end.


-spec parse_task_deps(map()) -> {ok, [binary()]} | {error, invalid_json}.

parse_task_deps(RawTask) ->
    case maps:find(<<"requires">>, RawTask) of
        error ->
            {ok, []};

        {ok, Deps} ->
            jp_utils:maybe_map(fun parse_task_dep/1, Deps)
    end.


-spec parse_task_dep(binary() | term()) -> {ok, binary()} | {error, invalid_json}.

parse_task_dep(Dep) when is_binary(Dep) ->
    {ok, Dep};
parse_task_dep(_) ->
    {error, invalid_json}.


-spec serialyse_tasks([task()], #{type := json | shell}) -> binary().

serialyse_tasks(Tasks, #{type := shell}) ->
    Commands = lists:map(fun get_task_command/1, Tasks),
    Script = iolist_to_binary(lists:join("\n", Commands)),
    <<"#!/usr/bin/env bash\n", Script/binary>>;

serialyse_tasks(Tasks, #{type := json}) ->
    Tasks1 = lists:map(fun task_to_map/1, Tasks),
    jsone:encode(#{
        <<"status">> => <<"ok">>,
        <<"tasks">> => Tasks1
    }).


-spec task_to_map(task()) -> map().

task_to_map(Task) ->
    #{
        <<"name">> => Task#task.name,
        <<"command">> => Task#task.command
     }.


-spec get_task_command(task()) -> binary().

get_task_command(Task) ->
    Task#task.command.


-spec serialyse_error(Error, #{type := json | shell}) -> binary() when
      Error :: invalid_json
             | body_limit_exceeded
             | jp_plan:err_reason().

serialyse_error(Error, #{type := json}) ->
    jsone:encode(#{
        <<"status">> => <<"error">>,
        <<"message">> => get_error_description(Error)
    });

serialyse_error(Error, #{type := shell}) ->
    <<$", (get_error_description(Error))/binary, $">>.


-spec get_error_description(Error) -> binary() when
      Error :: invalid_json
             | body_limit_exceeded
             | jp_plan:err_reason().

get_error_description({dependency_not_found, TaskName}) ->
    <<"Dependency not found: ", TaskName/binary>>;

get_error_description({cycle, TaskNames}) ->
    TaskNames1 = [[$", TaskName, $"] || TaskName <- TaskNames],
    Path = iolist_to_binary(lists:join("->", TaskNames1)),
    <<"Cycle detected: ", Path/binary>>;

get_error_description({duplicate, TaskName}) ->
    <<"Duplicate tasks: ", TaskName/binary>>;

get_error_description(invalid_json) ->
    <<"Invalid JSON">>;

get_error_description(body_limit_exceeded) ->
    <<"Body limit exceeded">>.
