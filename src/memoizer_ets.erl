%% @author Zoltán Lajos Kis <zoltan.lajos.kis@gmail.com>
%% @copyright 2016 Zoltán Lajos Kis
%%
%%
%% ETS-based memoization.
-module(memoizer_ets).
-export([memoize/2]).

-import(erl_syntax, [application/2, application/3, atom/1, atom_value/1,
                     attribute/2, case_expr/2, clause/3,  function/2,
                     function_arity/1, function_name/1, fun_expr/1, integer/1,
                     list/1, match_expr/2, receive_expr/1, receive_expr/3,
                     revert/1, tuple/1, underscore/0, variable/1]).

-define(ONLOAD_FUN, 'memoizer_onload').
-define(ONLOAD_INIT, 'memoizer_onload_init').
-define(ONLOAD_LOOP, 'memoizer_onload_loop').
-define(ONLOAD_REGNAME, '#memoizer').
-define(VALUE_VAR, "Value").
-define(VALUE_ATOM, 'value').

-type( fa() :: {atom(), arity()} ).


%% @doc Renames selected functions and creates memoized wrappers. Adds inline
%% flag for the renamed functions, and adds onload attribute and functions to
%% initialize ETS tables.
%% See `inline_funs', `onload_ets' and `memoize_fun' for details.
-spec( memoize(erl_syntax:forms(), [fa()]) -> erl_syntax:forms() ).
memoize(Forms, MzeFuns) ->
    Forms2 = inline_funs(Forms, MzeFuns),
    Forms3 = onload_ets(Forms2, MzeFuns),
    memoizer_syntax:flatmap_funs(fun memoize_fun/1, MzeFuns, Forms3).



%% @doc Adds a compile inline attribute for the memoized functions to the list
%% of forms.
-spec( inline_funs(erl_syntax:forms(), [fa()]) -> erl_syntax:forms() ).
inline_funs(Forms, MzeFuns) ->
    FAs = [tuple([ atom(memoizer_utils:mze_name(F)), integer(A) ])
                    || {F,A} <- MzeFuns],

    InlineAttr = revert(attribute(
                            atom('compile'), [
                            tuple([ atom('inline'), list(FAs) ])
                ])),

    memoizer_syntax:insert(Forms, [InlineAttr], 'function').



%% @doc Adds an onload attribute and a set of functions to initialize the ETS
%% tables.
-spec( onload_ets(erl_syntax:forms(), [fa()]) -> erl_syntax:forms() ).
onload_ets(Forms, MzeFuns) ->
    Attr = onload_attr(),
    Funs = [onload_fun(), onload_init(MzeFuns), onload_loop()],

    Forms2 = memoizer_syntax:insert(Forms, [Attr], 'function'),
    memoizer_syntax:insert(Forms2, Funs, 'function').



%% @doc Returns an onload attribute.
-spec( onload_attr() -> erl_syntax:syntaxTree() ).
onload_attr() ->
    revert(attribute(
                atom('on_load'), [
                tuple([ atom(?ONLOAD_FUN), integer(0) ])
    ])).



%% @doc Returns the onload function. This function runs when the module is
%% loaded. It spawns a process to execute onload_init.
-spec( onload_fun() -> erl_syntax:syntaxTree() ).
onload_fun() ->
    revert(function(
        atom(?ONLOAD_FUN), [
        clause([], 'none', [
            match_expr(
                variable("Self"),
                application(atom('erlang'), atom('self'), [])
            ),
            match_expr(
                variable("Pid"),
                application(
                    atom('erlang'), atom('spawn'), [
                    fun_expr([clause([], 'none', [
                        application(atom(?ONLOAD_INIT), [variable("Self")])
                    ])])
                ])
            ),
            receive_expr([
                clause([tuple([ variable("Pid"), variable("Res") ])], 'none', [
                    variable("Res")
                ])],
                integer(3000), [
                atom('timeout')
            ])
        ])
    ])).



%% @doc Returns the onload init function. It initializes the ETS tables, then
%% calls onload_loop.
-spec( onload_init([fa()]) -> erl_syntax:syntaxTree() ).
onload_init(MzeFuns) ->
    revert(function(
        atom(?ONLOAD_INIT), [
        clause([variable("OnLoadProc")], 'none', [
            application(atom('erlang'), atom('register'), [
                atom(?ONLOAD_REGNAME),
                application(atom('erlang'), atom('self'), [])
            ])]
            ++
            [onload_init_table(FA) || FA <- MzeFuns]
            ++ [
            application(atom('erlang'), atom('!'), [
                variable("OnLoadProc"),
                tuple([
                    application(atom('erlang'), atom('self'), []),
                    atom('ok')
                ])
            ]),
            application(atom(?ONLOAD_LOOP), [])
        ])
    ])).


%% @doc Returns the body of initializing one ETS table.
-spec( onload_init_table(fa()) -> erl_syntax:syntaxTree() ).
onload_init_table(FA) ->
    TblName = atom( memoizer_utils:mze_name(FA) ),

    case_expr(
        application(atom('ets'), atom('info'), [TblName]), [
        clause([atom('undefined')], 'none', [
            application(atom('ets'), atom('new'), [
                TblName,
                list([atom('named_table'), atom('set'), atom('public')])
            ])
        ]),
        clause([underscore()], 'none', [
            atom('ok')
        ])
    ]).



%% @doc Returns the onload loop function. This is an endless loop that keeps
%% the ETS owner process alive.
-spec( onload_loop() -> erl_syntax:syntaxTree() ).
onload_loop() ->
    revert(function(
        atom(?ONLOAD_LOOP), [
        clause([], 'none', [
            receive_expr([
                clause([underscore()], 'none', [
                    atom('ok')
                ])
            ]),
            application(atom(?ONLOAD_LOOP), [])
        ])
    ])).



%% @doc Turns a function form into memoized forms. The original function is
%% renamed, and a new memoized wrapper function is created.
%% See `mze_fun' for details.
-spec( memoize_fun(erl_syntax:syntaxTree()) -> [erl_syntax:syntaxTree()] ).
memoize_fun(Form) ->
    Name    = atom_value( function_name(Form) ),
    Arity   = function_arity(Form),
    MzeName = memoizer_utils:mze_name(Name),

    [ memoizer_syntax:rename(Form, MzeName), mze_fun(Name, Arity) ].



%% @doc Creates a memoization wrapper function.
-spec( mze_fun(atom(), arity()) -> erl_syntax:syntaxTree() ).
mze_fun(Name, Arity) ->
    TblName  = atom( memoizer_utils:mze_name({Name, Arity}) ),
    MzeName  = atom( memoizer_utils:mze_name(Name) ),
    VarList  = memoizer_syntax:var_list(memoizer_utils:var_list(Arity)),
    ValueVar = variable(?VALUE_VAR),

    revert(function(
        atom(Name), [
        clause(VarList, 'none', [
            case_expr(
                application(atom('ets'), atom('lookup'), [
                    TblName, tuple(VarList)
                ]), [
                clause([list([tuple([underscore(), ValueVar])])], 'none', [
                    ValueVar
                ]),
                clause([list([])], 'none', [
                    match_expr( ValueVar, application(MzeName, VarList) ),
                    application(atom('ets'), atom('insert'), [
                        TblName,
                        tuple([ tuple(VarList), ValueVar ])
                    ]),
                    ValueVar
                ])
            ])
        ])
    ])).

