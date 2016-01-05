%% @author Zoltán Lajos Kis <zoltan.lajos.kis@gmail.com>
%% @copyright 2016 Zoltán Lajos Kis
%%
%%
%% @doc Main module of Memoizer.
-module(memoizer).
-export([parse_transform/2]).

-import(erl_syntax, [atom_value/1, attribute_arguments/1, attribute_name/1,
                     concrete/1, function_arity/1, function_name/1, is_atom/2,
                     list_elements/1, type/1]).

-define(ATTRIBUTE_NAME, 'memoize').
-define(METHODS,        ['pd', 'ets']).

-type( fa()        :: {atom(), arity()} ).
-type( option()    :: atom() | {atom(), term()} ).
-type (mzemethod() :: 'pd' | 'ets').
-type( mzefa()     :: {fa(), mzemethod() } ).


%% @doc A parse transform that memoizes selected functions.
-spec( parse_transform(erl_syntax:forms(), [option()]) -> erl_syntax:forms() ).
parse_transform(Forms, _Opts) ->
    AttrFuns = lists:flatmap(fun parse_attr/1, Forms),
    FormFuns = lists:filtermap(fun parse_fun/1, Forms),
    MzeFuns = valid_funs(AttrFuns, FormFuns),

    PDFuns = [FA || {FA, 'pd'} <- MzeFuns],
    Forms2 = memoizer_pd:memoize(Forms, PDFuns),

    ETSFuns = [FA || {FA, 'ets'} <- MzeFuns],
    memoizer_ets:memoize(Forms2, ETSFuns).



%% @doc Returns the list of functions defined in the forms, in a format
%% compatible with lists:filtermap.
-spec( parse_fun(erl_syntax:syntaxTree()) -> {'true', mzefa()} | 'false' ).
parse_fun(Form) ->
    case type(Form) of
        'function' ->
            {true, {atom_value(function_name(Form)), function_arity(Form)}};
        _ ->
            false
    end.



%% @doc Returns the list of functions set in memoization attributes., in a
%% format compatible with lists:flatmap.
-spec( parse_attr(erl_syntax:syntaxTree()) -> [mzefa()] ).
parse_attr(Form) ->
    case type(Form) of
        'attribute' ->
            case atom_value(attribute_name(Form)) of
                ?ATTRIBUTE_NAME -> parse_attr_args(Form);
                _               -> []
            end;
        _ ->
            []
    end.



%% @doc Returns the list of functions and methods from a memoization attribute.
-spec( parse_attr_args(erl_syntax:syntaxTree()) -> [mzefa()] ).
parse_attr_args(Form) ->
    case attribute_arguments(Form) of
        'none'  -> io:format("Memoizer warning: empty attribute.~n"), [];
        [Arg|_] -> parse_attr_arg(Arg)
    end.



%% @doc Returns the list of selected functions and methods from a memoization
%% argument.
-spec( parse_attr_arg(erl_syntax:syntaxTree()) -> [mzefa()] ).
parse_attr_arg(Arg) ->
    case type(Arg) of
        'list'  ->
            lists:flatmap(fun parse_attr_arg/1, list_elements(Arg));
        'tuple' ->
            parse_attr_tuple(concrete(Arg));
        _       ->
            io:format("Memoizer warning: unknown attribute (~p).~n", [Arg]),
            []
    end.



%% @doc Validates the selected method and returns the function to be memoized
%% from an argument.
-spec( parse_attr_tuple({fa(), atom()}) -> [mzefa()] ).
parse_attr_tuple({{F, A}, Method} = MzeFA) when is_atom(F), is_integer(A),
                                                          is_atom(Method) ->
    case lists:member(Method, ?METHODS) of
        true ->
            [MzeFA];
        false ->
            io:format("Memoizer warning: unknown method (~p).~n", [Method]),
            []
    end;

parse_attr_tuple({{F, A} = FA}) when is_atom(F), is_integer(A) ->
    [{FA, hd(?METHODS)}];

parse_attr_tuple({F, A} = FA) when is_atom(F), is_integer(A) ->
    [{FA, hd(?METHODS)}];

parse_attr_tuple(Tuple) ->
    io:format("Memoizer warning: unknown attribute (~p).~n", [Tuple]), [].



%% @doc Filters memoization functions that are not in the function list.
-spec( valid_funs([mzefa()], [fa()]) -> [mzefa()] ).
valid_funs([{FA, _Method} = MzeFA | Rest], FAs) ->
    case lists:member(FA, FAs) of
        true ->
            [ MzeFA | valid_funs(Rest, FAs) ];
        false ->
            io:format("Memoizer warning: unknown function (~p).~n", [FA]),
            valid_funs(Rest, FAs)
    end;

valid_funs([], _FAs) -> [].

