Memoizer
========
Memoizer is a [memoization](https://en.wikipedia.org/wiki/Memoization) library
for Erlang. It uses parse transformation to transparently convert functions
into their memoized versions.

Memoizer is capable of using process dictionaries or ETS tables to store
previous function call results.



Example
-------
This is a naive dynamic programming solution for [problem 15 of
Project Euler](https://projecteuler.net/index.php?section=problems&id=015).

```erlang
-module(euler15).
-export([solve/1]).

solve(Size) -> routes(Size, 0, 0).

routes(S, _X,  S) -> 1;
routes(S,  S, _Y) -> 1;
routes(S,  X,  Y) -> routes(S, X+1, Y) + routes(S, X, Y+1).
```

Running this code is extremely slow - even for smaller grids - as the recursive
calls to `routes/2` are re-evaluated each and every time:
```erlang
1> timer:tc(euler15, solve, [16]).
{18107616,601080390}
```

Let's use Memoizer to transform the function.
```erlang
-module(euler15).
-export([solve/1]).

-compile({parse_transform, memoizer}).
-memoize(routes/3).

solve(Size) -> routes(Size, 0, 0).

routes(S, _X,  S) -> 1;
routes(S,  S, _Y) -> 1;
routes(S,  X,  Y) -> routes(S, X+1, Y) + routes(S, X, Y+1).
```

Running the code now takes almost no time, as expected:
```erl
1> timer:tc(euler15, solve, [20]).
{1221,137846528820}
```



Usage
-----
To use Memoizer, it must be specified as a parse transformation for the module:
```erlang
-compile({parse_transform, memoizer}).
```

Each function to be memoized must be listed with the `memoize` attribute. The
attribute can contain a single element or a list of elements, and can be used
multiple times. The memoization method can be set for each function. Use `pd`
for process dictionary or `ets` for ETS tables. By default process dictionary
method is used.
```erlang
-memoize(fun1/0).
-memoize({fun2/1, 'ets'}).
-memoize([fun3/2, {fun4/3, 'ets'}]).
```

Memoizer beam files must be available when compiling other modules.
When using rebar, it can be specified as a dependency in `rebar.conf`:
```erlang
%% ...
{deps, [
  %% ...
  {memoizer, ".*", {git, "https://github.com/ZoltanLajosKis/erl-memoizer"}}
]}.
```

Then the application can be compiled with rebar as usual:
```shell
rebar get-deps
rebar compile
```



Process dictionary method
-------------------------
This method is preferred when a long-running process calls the same function
multiple times as it avoids locking or any coordination with other processes.
This method is not useful however for highly distributed processing, as the
previous results are not shared among processes.

During compilation the process dictionary method transforms the following
module code ...
```erlang
-module(example).
-export(solve/1).

-compile({parse_transform, memoizer}).
-memoize(routes/3).

solve(Size) -> routes(Size, 0, 0).

routes(S, _X,  S) -> 1;
routes(S,  S, _Y) -> 1;
routes(S,  X,  Y) -> routes(S, X+1, Y) + routes(S, X, Y+1).
```

... into this code:
```erlang
-module(example).
-export(solve/1).

-compile({parse_transform, memoizer}).
-memoize(routes/3).

solve(Size) -> routes(Size, 0, 0).

routes(Var1, Var2, Var3) ->
    case erlang:get({'#routes_memoized', Var1, Var2, Var3}) of
        {'value', Value} ->
            Value;
        'undefined' ->
            Value = '#routes_memoized'(Var1, Var2, Var3),
            erlang:put({'#routes_memoized', Var1, Var2, Var3}, {'value', Value}),
            Value
    end.

'#routes_memoized'(S, _X,  S) -> 1;
'#routes_memoized'(S,  S, _Y) -> 1;
'#routes_memoized'(S,  X,  Y) -> routes(S, X+1, Y) + routes(S, X, Y+1).
```


ETS method
----------
This method is preferred for distributed processing, where multiple processes
need to share the previous results of function calls.

The current implementation does not play well with module code
upgrades (this is the cost of fully transparent memoization).

During compilation the ETS method transforms the following module code ...
```erlang
-module(example).
-export(solve/1).

-compile({parse_transform, memoizer}).
-memoize(routes/3).

solve(Size) -> routes(Size, 0, 0).

routes(S, _X,  S) -> 1;
routes(S,  S, _Y) -> 1;
routes(S,  X,  Y) -> routes(S, X+1, Y) + routes(S, X, Y+1).
```

... into this code:
```erlang
-module(example).
-export(solve/1).

-compile({parse_transform, memoizer}).
-memoize({routes/3, 'ets'}).

-on_load(memoizer_onload/0).

memoizer_onload() ->
    Self = erlang:self(),
    Pid = erlang:spawn(fun() -> memoizer_onload_init(Self) end),
    receive
        {Pid, Result} -> Result
        after 3000    -> 'timeout'
    end.

memoizer_onload_init(OnLoadProc) ->
    case ets:info('#routes_3_memoized') of
        'undefined' ->
            ets:new('#routes_3_memoized', ['named_table', 'set', 'public']);
        _ ->
            ok
    end,
    OnLoadProc ! {self(), 'ok'},
    memoizer_onload_loop().

memoizer_onload_loop() ->
    receive
        _ -> ok
    end,
    memoizer_onload_loop().

solve(Size) -> routes(Size, 0, 0).

routes(Var1, Var2, Var3) ->
    case ets:lookup('#routes_3_memoized', {Var1, Var2, Var3}) of
        [Value] ->
            Value;
        [] ->
            Value = '#routes_memoized'(Var1, Var2, Var3),
            ets:insert('#routes_3_memoized', {{Var1, Var2, Var3}, Value}),
            Value
    end.

'#routes_memoized'(S, _X,  S) -> 1;
'#routes_memoized'(S,  S, _Y) -> 1;
'#routes_memoized'(S,  X,  Y) -> routes(S, X+1, Y) + routes(S, X, Y+1).
```

