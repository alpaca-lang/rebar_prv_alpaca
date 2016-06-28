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


{provider_hooks, [{post, [{compile, {mlfe, compile}}]}]}.
```



```
$ rebar3 shell
===> Fetching rebar_prv_mlfe
===> Compiling rebar_prv_mlfe
_checkouts/rebar_prv_mlfe/src/rebar_prv_mlfe_compile.erl:43: Warning: variable 'ModuleName' is unused

===> Compiling rebar_prv_mlfe
===> Compiling rebar_prv_mlfe
===> Verifying dependencies...
===> Compiling mlfe_test
Env is [{[108,101,110],1}]

Compiling apply for len env is [{[108,101,110],1}]

FORMS
{c_module,[],{c_literal,[],basic_adt},[{c_var,[],{len,1}}],[],[{{c_var,[],{len,1}},{c_fun,[],[{c_var,[],svar_0}],{c_case,[],{c_var,[],svar_0},[{c_clause,[],[{c_literal,[],'Nil'}],{c_literal,[],true},{c_literal,[],0}},{c_clause,[],[{c_tuple,[],[{c_literal,[],'Cons'},{c_tuple,[],[{c_var,[],[95]},{c_var,[],tail}]}]}],{c_literal,[],true},{c_call,[],{c_literal,[],erlang},{c_literal,[],'+'},[{c_literal,[],1},{c_apply,[],{c_var,[],{len,1}},[{c_var,[],tail}]}]}}]}}}]}
Erlang/OTP 19 [erts-8.0] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.0  (abort with ^G)
1> r3:do(compile).
Verifying dependencies...
Compiling mlfe_test
Env is [{[108,101,110],1}]

Compiling apply for len env is [{[108,101,110],1}]

FORMS
{c_module,[],{c_literal,[],basic_adt},[{c_var,[],{len,1}}],[],[{{c_var,[],{len,1}},{c_fun,[],[{c_var,[],svar_0}],{c_case,[],{c_var,[],svar_0},[{c_clause,[],[{c_literal,[],'Nil'}],{c_literal,[],true},{c_literal,[],0}},{c_clause,[],[{c_tuple,[],[{c_literal,[],'Cons'},{c_tuple,[],[{c_var,[],[95]},{c_var,[],tail}]}]}],{c_literal,[],true},{c_call,[],{c_literal,[],erlang},{c_literal,[],'+'},[{c_literal,[],1},{c_apply,[],{c_var,[],{len,1}},[{c_var,[],tail}]}]}}]}}}]}
ok
2>
```
