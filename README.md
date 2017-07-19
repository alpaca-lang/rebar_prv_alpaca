rebar_prv_alpaca
=====

rebar3 plugin for compiling [alpaca](https://github.com/alpaca-lang/alpaca) modules.

Use
---

From 0.2.8, Alpaca must be installed. Please see instructions at http://alpaca-lang.org.

Add the plugin to your rebar config:

```
{plugins, [
    {rebar_prv_alpaca, ".*", {git, "https://github.com/alpaca/rebar_prv_alpaca.git", {branch, "master"}}}
]}.


{provider_hooks, [{post, [{compile, {alpaca, compile}}]}]}.
```


Options
---

Options can be passed to the Alpaca compiler via a `{rebar_prv_alpaca, ... }` section in your rebar.config
file. Currently, the only option supported is for default imports (i.e. functions and types that can be
implictly included in all your modules), e.g.

```
{rebar_prv_alpaca, 
	[ { default_imports, [ {default, list_defaults} ] } ]
}.
```

`default_imports` takes a list of tuples of `{Module, Function}`. In the instance above
it might live in a module that looks like this:

```
-module(default).
-export([list_defaults/0]).

list_defaults() ->
    Funs = [{assert, <<"equal">>}],
    Types = [{utils, <<"Result">>}],
    {Funs, Types}.
```

The `list_defaults/0` function returns a tuple of {[FunctionRef], [TypeRef]}.
These functions and types will then be injected into each of your modules.

The intention behind this is that most of the time you will simply wish to
reference a "default" set of imports from Alpaca's own standard library (which
is a work in prgress) but that ultimately you could pull in other standard
libraries or save on explicitly importing functions and types you use 
everywhere.
