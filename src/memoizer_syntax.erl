%% @author Zoltán Lajos Kis <zoltan.lajos.kis@gmail.com>
%% @copyright 2016 Zoltán Lajos Kis
%%
%%
%% @doc Functions for syntax tree manipulation.
-module(memoizer_syntax).
-export([flatmap_funs/3, insert/3, rename/2, var_list/1]).

-import(erl_syntax, [atom/1, atom_value/1, function/2, function_arity/1,
                     function_clauses/1, function_name/1, revert/1, type/1,
                     variable/1]).

-type( fa() :: {atom(), arity()} ).


%% @doc Flatmaps the function on matching function forms; leaves other forms
%% unchanged.
-spec( flatmap_funs(fun((erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree()),
            [fa()], erl_syntax:forms()) -> erl_syntax:forms() ).
flatmap_funs(Fun, OnFuns, Forms) ->
    lists:flatmap( fun(Form) ->
        case type(Form) of
            'function' ->
                Name = atom_value(function_name(Form)),
                Arity = function_arity(Form),
                case lists:member({Name, Arity}, OnFuns) of
                    'true' -> Fun(Form);
                    'false' -> [Form]
                end;
            _ -> [Form]
        end
        end, Forms).



%% @doc Inserts new forms into a list of forms before the first occurence
%% of the specified form type.
-spec( insert(erl_syntax:forms(), [erl_syntax:syntaxTree()], atom())
                                                    -> erl_syntax:forms() ).
insert([Form|Rest] = Forms, NewForms, Type) ->
    case type(Form) of
        Type -> NewForms ++ Forms;
        _    -> [Form | insert(Rest, NewForms, Type)]
    end;

insert([], NewForms, _Type) ->
    NewForms.



%% @doc Renames the function in the form and returns the new form.
-spec( rename(erl_syntax:syntaxTree(), atom() | string()) ->
                                                   erl_syntax:syntaxTree() ).
rename(Form, NewName) ->
    case type(Form) of
        'function' ->
            revert(function(
                        atom(NewName),
                        function_clauses(Form)
            ));
        _ ->
            Form
    end.



%% @doc Creates a list of variable nodes from the list of names.
-spec (var_list([string()]) -> [erl_syntax:syntaxTree()] ).
var_list(VarNames) ->
    [variable(N) || N <- VarNames].

