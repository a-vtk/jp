-module(jp_app).
-behaviour(application).

%% API
-export([
    get_http_port/0,
    get_env/1,
    get_env/2
]).


%% application callbacks
-export([
    start/2,
    stop/1
]).


%% application callbacks

-spec start(StartType,  StartArgs) -> Result when
        StartType :: application:start_type(),
        StartArgs :: term(),
        Result :: {'ok', pid()}
                | {'ok', pid(), State :: term()}
                | {'error', Reason :: term()}.

start(_StartType, _StartArgs) ->
    HttpConfig = get_env(http_listener),

    Dispatch = cowboy_router:compile([{'_', [
        {"/api/v1/plans", jp_http_plans_h, #{}}
    ]}]),

    {ok, _} = cowboy:start_clear(jp_listener_http,
        [
            {port, maps:get(port, HttpConfig)}
        ],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [cowboy_router, cowboy_handler]
        }
    ),
    jp_sup:start_link().


-spec stop(State :: term()) -> term().

stop(_State) ->
    ok.

%% API

-spec get_http_port() -> undefined | integer().

get_http_port() ->
    ranch:get_port(jp_listener_http).


-spec get_env(Key :: atom()) -> undefined | term().

get_env(Key) ->
    get_env(Key, undefined).


-spec get_env(Key, Default) -> Default | Value when
      Key :: atom(),
      Default :: term(),
      Value :: term().

get_env(Key, Default) ->
    application:get_env(job_planner, Key, Default).
