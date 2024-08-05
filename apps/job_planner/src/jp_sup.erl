-module(jp_sup).
-behaviour(supervisor).

-export([
    start_link/0
]).

-export([
    init/1
]).

-define(SERVER, ?MODULE).

%% API

-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% supervisor callbacks


-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    Flags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    Children = [],
    {ok, {Flags, Children}}.
