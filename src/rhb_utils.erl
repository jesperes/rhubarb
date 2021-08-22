%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2021, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 22 Aug 2021 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(rhb_utils).

-include_lib("stdlib/include/assert.hrl").

-export([
    with_tmpfile/1,
    with_tmpfile/2,
    in_tmpdir/1
]).

-spec with_tmpfile(fun((file:filename()) -> any())) -> any().
with_tmpfile(Fun) ->
    F = create_tmpfile(),
    try
        Fun(F)
    after
        file:delete(F)
    end.

-spec with_tmpfile(Contents :: file:iodata(), fun((file:filename()) -> any())) -> any().
with_tmpfile(Contents, Fun) ->
    F = create_tmpfile(),
    ok = file:write_file(F, Contents),
    try
        ?assert(filelib:is_file(F)),
        Fun(F)
    after
        file:delete(F)
    end.

in_tmpdir(Fun) ->
    {ok, Cwd} = file:get_cwd(),
    D = create_tmpfile(),
    filelib:ensure_dir(filename:join(D, "x")),
    file:set_cwd(D),
    try
        Fun()
    after
        file:set_cwd(Cwd),
        file:del_dir_r(D)
    end.

%% Internal functions
-spec create_tmpfile() -> file:filename().
create_tmpfile() ->
    {A, B, C} = erlang:timestamp(),
    N = node(),
    lists:flatten(io_lib:format("/tmp/~p-~p.~p.~p", [N, A, B, C])).
