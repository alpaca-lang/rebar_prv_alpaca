rebar_prv_mlfe
=====

rebar3 plugin for compiling [mlfe](https://github.com/j14159/mlfe) modules.

Use
---

Add the plugin to your rebar config:

```
{plugins, [
    {rebar_prv_mlfe, ".*", {git, "https://github.com/tsloughter/rebar_prv_mlfe.git", {branch, "master"}}}
]}.
```

```
$ rebar3 mlfe compile
===> Fetching rebar_prv_mlfe
===> Compiling rebar_prv_mlfe
<Plugin Output>
```
