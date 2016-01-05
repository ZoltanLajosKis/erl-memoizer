%% @author Zoltán Lajos Kis <zoltan.lajos.kis@gmail.com>
%% @copyright 2016 Zoltán Lajos Kis
%%
%%
%% Process dictionary-based memoization.
-module(memoizer_pd).
-export([memoize/2]).

-import(erl_syntax, [application/2, application/3, atom/1, atom_value/1,
                     attribute/2, case_expr/2, clause/3,  function/2,
                     function_arity/1, function_name/1, integer/1, list/1,
                     match_expr/2, revert/1, tuple/1, variable/1]).

-define(VALUE_VAR, "Value").
-define(VALUE_ATOM, 'value').

-type( fa() :: {atom(), arity()} ).

%% @doc Renames selected functions and creates memoized wrappers. Adds inline
%% flag for the renamed functions.
%% See `inline_funs' and `memoize_fun' for details.
-spec( memoize(erl_syntax:forms(), [fa()]) -> erl_syntax:forms() ).
memoize(Forms, MzeFuns) ->
    Forms2 = inline_funs(Forms, MzeFuns),
    memoizer_syntax:flatmap_funs(fun memoize_fun/1, MzeFuns, Forms2).



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
    MzeName    = atom( memoizer_utils:mze_name(Name) ),
    VarList    = memoizer_syntax:var_list(memoizer_utils:var_list(Arity)),
    ValueVar   = variable(?VALUE_VAR),
    ValueTuple = tuple([ atom(?VALUE_ATOM), ValueVar ]),

    revert(function(
        atom(Name), [
        clause(VarList, 'none', [
            case_expr(
                application(atom('erlang'), atom('get'), [
                    tuple([ MzeName | VarList ])
                ]), [
                clause([ValueTuple], 'none', [
                    ValueVar
                ]),
                clause([atom('undefined')], 'none', [
                    match_expr( ValueVar, application(MzeName, VarList) ),
                    application(atom('erlang'), atom('put'), [
                        tuple([ MzeName | VarList ]),
                        ValueTuple
                    ]),
                    ValueVar
                ])
            ])
        ])
    ])).

