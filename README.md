rebar_prv_alpaca
=====

rebar3 plugin for compiling [alpaca](https://github.com/alpaca-lang/alpaca) modules.

Use
---

Add the plugin to your rebar config:

```
{plugins, [
    {rebar_prv_alpaca, ".*", {git, "https://github.com/tsloughter/rebar_prv_alpaca.git", {branch, "master"}}}
]}.


{provider_hooks, [{post, [{compile, {alpaca, compile}}]}]}.
```



```
$ rebar3 alpaca shell
...
 == Alpaca Shell 0.02a ==

 (hint: exit with ctrl-c, run expression by terminating with ';;' or an empty line)

 -> add x y = x + y ;;
 -> add 10 20 ;;
-- 30
```
