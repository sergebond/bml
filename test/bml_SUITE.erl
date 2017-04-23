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
  [my_test_case].


my_test_case(Config) ->
  FilePath = ?config(data_dir, Config) ++ "simple.html",
  {ok, BinaryFile} = file:read_file(FilePath),
  Size1 = size(BinaryFile),
  Size12 = size(zlib:gzip(BinaryFile)),
  Res = bml:decode(BinaryFile),
  BinaryTerm = term_to_binary(Res),
  Size2 = size(term_to_binary(Res)),
  ct:pal("~p", [Res]),
  Size3 = size(zlib:gzip(BinaryTerm)),

  ct:pal("Size1 ~p, Size12 ~p, Size2 ~p, Size3 ~p", [Size1, Size12, Size2, Size3]),
  ok.
