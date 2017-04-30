-module(bml_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

groups() ->
  [].

all() ->
  [
%%    my_test_case
    two_direction_test
  ].

my_test_case(Config) ->
  FilePath = ?config(data_dir, Config) ++ "simple.html",
  {ok, BinaryFile} = file:read_file(FilePath),
  Size1 = size(BinaryFile),
  Size12 = size(zlib:compress(BinaryFile)),
  Res = bml:encode(BinaryFile),
  Encoded = size(Res),
  ct:pal("Result is ~p", [Res]),
  EncodedZipped = (catch size(zlib:compress(Res))),
  ct:pal("RawSize ~p,~n RawZipped ~p, ~n EncodedSize ~p,~n EncodedZipped ~p", [Size1, Size12, Encoded, EncodedZipped] ),
  ok.

two_direction_test(Config) ->
  FilePath = ?config(data_dir, Config) ++ "simple.html",
  {ok, BinaryFile} = file:read_file(FilePath),
  ct:pal("~p", [BinaryFile]),
  Res0 = bml:encode(BinaryFile),
  ct:pal("Encoded is ~p", [Res0]),
  Res = decode:decode(1, Res0),
  ct:pal("+++Decoded is ~p", [Res]),
  Config.