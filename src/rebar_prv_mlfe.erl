-module(rebar_prv_mlfe).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_prv_mlfe_compile:init(State),
    {ok, State1}.
