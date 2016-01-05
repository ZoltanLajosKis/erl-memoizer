%% @author Zoltán Lajos Kis <zoltan.lajos.kis@gmail.com>
%% @copyright 2016 Zoltán Lajos Kis
%%
%%
%% @doc Common functions.
-module(memoizer_utils).
-export([mze_name/1, var_list/1]).

-define(NAME_POSTFIX, "_memoized").
-define(VAR_NAME, "Var").

-type( fa() :: {atom(), arity()} ).


%% @doc Returns the memoized name of a function.
-spec( mze_name(atom()|fa()) -> string() ).
mze_name(Name) when is_atom(Name) ->
    "#" ++ atom_to_list(Name) ++ ?NAME_POSTFIX;

mze_name({Name, Arity}) when is_atom(Name), is_integer(Arity) ->
    "#" ++ atom_to_list(Name) ++ "_" ++ integer_to_list(Arity) ++ ?NAME_POSTFIX.



%% @doc Returns a list of variable names that can be used in the AST.
-spec( var_list(arity()) -> [string()] ).
var_list(N) ->
    var_list(N, []).

var_list(0, Vars) ->
    Vars;

var_list(N, Vars) ->
    Name = ?VAR_NAME ++ integer_to_list(N),
    var_list(N-1, [Name | Vars]).

