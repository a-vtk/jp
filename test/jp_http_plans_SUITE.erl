-module(jp_http_plans_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    t_order/1,
    t_cycles/1,
    t_duplicates/1,
    t_unknown_dependency/1,
    t_invalid_input/1,
    t_shell_output/1
]).

-define(TASK(Name),
    #{<<"name">> => Name, <<"command">> => Name}
).
-define(TASK(Name, Requires),
    #{<<"name">> => Name, <<"command">> => Name, <<"requires">> => Requires}
).

%% ct callbacks

all() ->
    [element(1, F)
    || F <- ?MODULE:module_info(exports), string:left(atom_to_list(element(1, F)), 2) =:= "t_"].


init_per_suite(Config) ->
    application:load(job_planner),
    application:set_env(job_planner, http_listener, #{port => 0}),

    {ok, Apps} = application:ensure_all_started(job_planner),
    Port = jp_app:get_http_port(),
    [
        {apps, Apps},
        {port, Port},
        {in_type, json},
        {out_type, json}
    | Config].


end_per_suite(Config) ->
    Apps = proplists:get_value(apps, Config),
    lists:foreach(fun application:stop/1, Apps).


init_per_testcase(_Case, Config) ->
    Config.


end_per_testcase(_Case, Config) ->
    Config.

%% tests

t_order(Config) ->
    TestCases = [
        {[], []},
        {
            [?TASK(<<"a">>)],
            [?TASK(<<"a">>)]
        },
        {
            [?TASK(<<"a">>), ?TASK(<<"b">>, [<<"a">>])],
            [?TASK(<<"a">>), ?TASK(<<"b">>)]
        },
        {
            [?TASK(<<"b">>, [<<"a">>]), ?TASK(<<"a">>)],
            [?TASK(<<"a">>), ?TASK(<<"b">>)]
        },
        {
            [?TASK(<<"a">>), ?TASK(<<"b">>, [<<"a">>]), ?TASK(<<"c">>, [<<"b">>])],
            [?TASK(<<"a">>), ?TASK(<<"b">>), ?TASK(<<"c">>)]
        },
        {
            [?TASK(<<"b">>, [<<"a">>]), ?TASK(<<"a">>), ?TASK(<<"c">>, [<<"b">>])],
            [?TASK(<<"a">>), ?TASK(<<"b">>), ?TASK(<<"c">>)]
        },
        {
            [?TASK(<<"c">>, [<<"b">>]), ?TASK(<<"a">>), ?TASK(<<"b">>, [<<"a">>])],
            [?TASK(<<"a">>), ?TASK(<<"b">>), ?TASK(<<"c">>)]
        },
        {
            [?TASK(<<"b">>, [<<"a">>]), ?TASK(<<"c">>, [<<"b">>]), ?TASK(<<"a">>)],
            [?TASK(<<"a">>), ?TASK(<<"b">>), ?TASK(<<"c">>)]
        },
        {
            [?TASK(<<"c">>, [<<"b">>]), ?TASK(<<"b">>, [<<"a">>]), ?TASK(<<"a">>)],
            [?TASK(<<"a">>), ?TASK(<<"b">>), ?TASK(<<"c">>)]
        },
        {
            [?TASK(<<"d">>, [<<"c">>]), ?TASK(<<"c">>, [<<"b">>]), ?TASK(<<"b">>, [<<"a">>]), ?TASK(<<"a">>)],
            [?TASK(<<"a">>), ?TASK(<<"b">>), ?TASK(<<"c">>), ?TASK(<<"d">>)]
        },
        {
            [
                ?TASK(<<"a">>, [<<"b">>]),
                ?TASK(<<"b">>),
                ?TASK(<<"c">>, [<<"a">>, <<"d">>]),
                ?TASK(<<"d">>, [<<"b">>]),
                ?TASK(<<"e">>, [<<"c">>]),
                ?TASK(<<"f">>, [<<"c">>])
            ],
            [
                ?TASK(<<"b">>),
                ?TASK(<<"a">>),
                ?TASK(<<"d">>),
                ?TASK(<<"c">>),
                ?TASK(<<"e">>),
                ?TASK(<<"f">>)
            ]
        },
        {
            [
                ?TASK(<<"a">>, [<<"b">>]),
                ?TASK(<<"c">>, [<<"a">>, <<"d">>]),
                ?TASK(<<"e">>, [<<"c">>]),
                ?TASK(<<"b">>),
                ?TASK(<<"d">>, [<<"b">>]),
                ?TASK(<<"f">>, [<<"c">>])
            ],
            [
                ?TASK(<<"b">>),
                ?TASK(<<"a">>),
                ?TASK(<<"d">>),
                ?TASK(<<"c">>),
                ?TASK(<<"e">>),
                ?TASK(<<"f">>)
            ]
        }
    ],

    lists:foreach(fun ({In, Out}) ->
        TestDescription = [{maps:get(<<"name">>, Ini), maps:get(<<"requires">>, Ini, [])} || Ini <- In],
        ct:pal("In: ~tp", [TestDescription]),

        ?assertMatch(
            {ok, {200, _, #{<<"status">> := <<"ok">>, <<"tasks">> := Out}}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, Config)
        )
    end, TestCases).


t_cycles(Config) ->
    TestCases = [
        {[?TASK(<<"a">>, [<<"a">>])], ["a", "a"]},
        {[?TASK(<<"a">>, [<<"b">>]), ?TASK(<<"b">>, [<<"a">>])], ["a", "b", "a"]},
        {
            [
              ?TASK(<<"a">>),
              ?TASK(<<"b">>, [<<"c">>]),
              ?TASK(<<"c">>, [<<"d">>]),
              ?TASK(<<"d">>, [<<"e">>]),
              ?TASK(<<"e">>, [<<"f">>]),
              ?TASK(<<"f">>, [<<"g">>]),
              ?TASK(<<"g">>, [<<"d">>])
            ],
            ["d", "e", "f", "g", "d"]
        }
    ],
    lists:foreach(fun ({In, Path}) ->
        TestDescription = [{maps:get(<<"name">>, Ini), maps:get(<<"requires">>, Ini, [])} || Ini <- In],
        ct:pal("In: ~tp", [TestDescription]),

        Path1 = iolist_to_binary(lists:join("->", [[$", X, $"] || X <- Path])),
        Message = <<"Cycle detected: ", Path1/binary>>,
        ?assertMatch(
            {ok, {400, _, #{<<"status">> := <<"error">>, <<"message">> := Message}}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, Config)
        ),
        Message1 = <<$", Message/binary, $">>,
        ?assertMatch(
            {ok, {400, _, Message1}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, [{out_type, shell} | Config])
        )
    end, TestCases).


t_duplicates(Config) ->
    TestCases = [
        {[?TASK(<<"a">>), ?TASK(<<"a">>)], "a"},
        {[?TASK(<<"a">>), ?TASK(<<"b">>), ?TASK(<<"c">>), ?TASK(<<"b">>)], "b"}
    ],
    lists:foreach(fun ({In, Duplicate}) ->
        TestDescription = [{maps:get(<<"name">>, Ini), maps:get(<<"requires">>, Ini, [])} || Ini <- In],
        ct:pal("In: ~tp", [TestDescription]),

        Duplicate1 = iolist_to_binary(Duplicate),
        Message = <<"Duplicate tasks: ", Duplicate1/binary>>,
        ?assertMatch(
            {ok, {400, _, #{<<"status">> := <<"error">>, <<"message">> := Message}}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, Config)
        ),
        Message1 = <<$", Message/binary, $">>,
        ?assertMatch(
            {ok, {400, _, Message1}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, [{out_type, shell} | Config])
        )
    end, TestCases).


t_unknown_dependency(Config) ->
    TestCases = [
        {[?TASK(<<"a">>, [<<"b">>])], "b"},
        {[?TASK(<<"a">>), ?TASK(<<"b">>, [<<"g">>]), ?TASK(<<"c">>, [<<"x">>])], "g"}
    ],
    lists:foreach(fun ({In, NotFound}) ->
        TestDescription = [{maps:get(<<"name">>, Ini), maps:get(<<"requires">>, Ini, [])} || Ini <- In],
        ct:pal("In: ~tp", [TestDescription]),

        NotFound1 = iolist_to_binary(NotFound),
        Message = <<"Dependency not found: ", NotFound1/binary>>,
        ?assertMatch(
            {ok, {400, _, #{<<"status">> := <<"error">>, <<"message">> := Message}}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, Config)
        ),
        Message1 = <<$", Message/binary, $">>,
        ?assertMatch(
            {ok, {400, _, Message1}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, [{out_type, shell} | Config])
        )
    end, TestCases).


t_invalid_input(Config) ->
    TestCases = [
        [?TASK(<<"a">>, [1])],
        [?TASK(<<"a">>), ?TASK(<<"b">>, [<<"g">>]), ?TASK(<<"c">>, [2])],
        "asdf",
        [#{}],
        [#{<<"command">> => <<"a">>}],
        [#{<<"requires">> => [<<"a">>]}],
        [#{<<"command">> => <<"a">>, <<"requires">> => [<<"a">>]}],
        ?TASK(1),
        [?TASK(1)]
    ],
    lists:foreach(fun (In) ->
        ct:pal("In: ~tp", [In]),

        Message = <<"Invalid JSON">>,
        ?assertMatch(
            {ok, {400, _, #{<<"status">> := <<"error">>, <<"message">> := Message}}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, Config)
        ),
        Message1 = <<$", Message/binary, $">>,
        ?assertMatch(
            {ok, {400, _, Message1}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, [{out_type, shell} | Config])
        )
    end, TestCases).


t_shell_output(Config0) ->
    Config = [{out_type, shell} | Config0],
    TestCases = [
        {[], []},
        {[?TASK(<<"a">>)], ["a"]},
        {[?TASK(<<"a">>), ?TASK(<<"b">>, [<<"a">>])], ["a", "b"]},
        {
            [?TASK(<<"b">>, [<<"a">>]), ?TASK(<<"a">>), ?TASK(<<"c">>, [<<"b">>])],
            ["a", "b", "c"]
        }
    ],

    lists:foreach(fun ({In, Out}) ->
        TestDescription = [{maps:get(<<"name">>, Ini), maps:get(<<"requires">>, Ini, [])} || Ini <- In],
        ct:pal("In: ~tp", [TestDescription]),

        ScriptHead = <<"#!/usr/bin/env bash\n">>,
        ScriptBody = iolist_to_binary(lists:join(<<"\n">>, Out)),
        Script = <<ScriptHead/binary, ScriptBody/binary>>,

        ?assertMatch(
            {ok, {200, _, Script}},
            httpc_post("/api/v1/plans", #{<<"tasks">> => In}, Config)
        )
    end, TestCases).

%% internal functions

httpc_post(Path, Data, Config) ->
    Url = get_url(Path, Config),

    InType = proplists:get_value(in_type, Config),
    OutType = proplists:get_value(out_type, Config),

    Headers = [
        {"Accept", mime_type(OutType)}
    ],
    ContentType = mime_type(InType),
    Body = encode(InType, Data),

    case httpc:request(post, {Url, Headers, ContentType, Body}, [], [{body_format, binary}]) of
        {ok, {{_, Code, _}, ResultHeaders, ResultBody}} ->
            {ok, {Code, ResultHeaders, decode(OutType, ResultBody)}}
    end.


get_url(Path, Config) ->
    Port = proplists:get_value(port, Config),
    "http://localhost:" ++ integer_to_list(Port) ++ Path.


mime_type(json)  -> "application/json";
mime_type(shell) -> "application/x-sh";
mime_type(xml)   -> "application/xml".


encode(json, Data) ->
    jsone:encode(Data).


decode(json, Data) ->
    jsone:decode(Data);
decode(shell, Data) ->
    Data.
