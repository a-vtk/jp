-module(jp_utils).
-feature(maybe_expr, enable).

-export([
    maybe_map/2,
    maybe_foldl/3
]).

%% API

-spec maybe_map(Fun, Xs1) -> {ok, Xs2} | {error, term()} when
    Fun :: fun((X1) -> {ok, X2} | {error, term()}),
    Xs1 :: [X1],
    Xs2 :: [X2],
    X1 :: term(),
    X2 :: term().

maybe_map(_Fun, []) ->
    {ok, []};
maybe_map(Fun, [H | T]) ->
    maybe
        {ok, H1} ?= Fun(H),
        {ok, T1} ?= maybe_map(Fun, T),

        {ok, [H1 | T1]}
    end.


-spec maybe_foldl(Fun, Acc1, Xs) -> {ok, Acc2} | {error ,term()} when
    Fun :: fun((X, AccIn) -> {ok, AccOut} | {error, term()}),
    X :: term(),
    Xs :: [X],
    Acc1 :: term(),
    Acc2 :: term(),
    AccIn :: term(),
    AccOut :: term().

maybe_foldl(_Fun, Acc, []) ->
    {ok, Acc};
maybe_foldl(Fun, Acc, [H | T]) ->
    maybe
        {ok, Acc1} ?= Fun(H, Acc),
        maybe_foldl(Fun, Acc1, T)
    end.
