-module(rebar_prv_alpaca).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_prv_alpaca_shell:init(State),
    {ok, State2} = rebar_prv_alpaca_compile:init(State1),
    {ok, State2}.
