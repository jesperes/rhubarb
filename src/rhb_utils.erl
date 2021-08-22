%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2021, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 22 Aug 2021 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(rhb_utils).

-export([with_tmpfile/1]).

-spec with_tmpfile(fun((file:filename()) -> any())) -> any().
with_tmpfile(Fun) ->
    F = create_tmpfile(),
    try
        Fun(F)
    after
        file:delete(F)
    end.

-spec create_tmpfile() -> file:filename().
create_tmpfile() ->
    {A, B, C} = erlang:timestamp(),
    N = node(),
    lists:flatten(io_lib:format("/tmp/~p-~p.~p.~p", [N, A, B, C])).
