%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2021, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 22 Aug 2021 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(rhb_scandeps).

-include_lib("eunit/include/eunit.hrl").

-export([scan/1]).

%% Scan an erlang source file for dependencies
-spec scan( file:name_all()
          | binary()
          | string()) ->
          list(
            {module, atom(), file:name()}
            | {file, file:name()}
           ).
scan(File) ->
    {ok, Form} = epp:parse_file(File, []),

    Deps0 = scan0(Form, sets:new()),

    %% Remove the dependency on itself
    Deps1 = sets:del_element({file, File}, Deps0),

    Deps2 = sets:to_list(Deps1),

    %% Lookup filenames for module dependencies
    lists:filtermap(
      fun
          ({module, Mod}) ->
              case code:which(Mod) of
                  preloaded ->
                      %% ignore preloaded modules
                      false;
                  cover_compiled ->
                      {true, {module, Mod}};
                  non_existing ->
                      %% Loadable beam file cannot be found
                      {true, {module, Mod, non_existing}};
                  Filename ->
                      {true, {module, Mod, Filename}}
              end;
          (Other) ->
              {true, Other}
      end,
      Deps2
     ).

-spec scan0(list(tuple()), sets:set()) -> sets:set().
scan0([], Deps) ->
    Deps;
scan0([{eof, _}], Deps) ->
    Deps;
scan0([{attribute, _, file, {FileName, _}} | Rest], Deps) ->
    scan0(Rest, sets:add_element({file, FileName}, Deps));
scan0([{attribute, _, compile, {parse_transform, PtMod}} | Rest], Deps) ->
    scan0(Rest, sets:add_element({module, PtMod}, Deps));
scan0([{attribute, _, behavior, Behavior} | Rest], Deps) ->
    scan0(Rest, sets:add_element({module, Behavior}, Deps));
scan0([_Other | Rest], Deps) ->
    scan0(Rest, Deps).

-ifdef(EUNIT).

%% Tests

scan_test() ->
    rhb_utils:in_tmpdir(
      fun() ->
              file:write_file("bad.erl", "-module(bad).\n"),
              {ok, bad} = compile:file("bad.erl"),
              file:write_file(
                "foobar.erl",
                "-include(\"foobar.hrl\").\n"
                "-behavior(bad).\n"
               ),
              file:write_file("foobar.hrl", "-define(FOO, bar)."),
              Deps = scan("foobar.erl"),
              ?assertNotEqual(Deps, [])
      end).

-endif.
