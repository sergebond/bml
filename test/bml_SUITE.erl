-module(bml_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-define(SAMPLE_1, <<"<html class=\"qwerty\" accesskey=\"dfrdrt\"><body><h1 align=\"top\">My First Heading</h1><p align=\"right\">My first paragraph.</p><p align=\"bottom\">My first parsavsdvsvdas</p><p align=\"top\">My fsv</p><form action=\"/cgi-bin/hello_get.cgi\" method=\"get\"><input type=\"text\" name=\"first_name\"/>Last name:<input type=\"text\" name=\"last_name\"/><input type=\"submit\" value=\"submit\"/></form><p align=\"bottom\">My jshv</p><p align=\"center\">My oshv</p><p align=\"left\">My dlfuhg;aoeirbh;oeij</p><form action=\"/cgi-bin/hello_get.cgi\" method=\"get\">First name:<input type=\"text\" name=\"first_name\"/>Last name:<input type=\"text\" name=\"last_name\"/><input type=\"submit\" value=\"submit\"/></form><p align=\"left\">skdjdbvb;kjsbddddddsdjjjjjjjjjjjjjjjjjjjjjjjjjjjvklJSKKDv'LKDNV;LJLJBDV;kjsvbk;vb;sdDJKvdnN;LNVD;VJBSVBk;sdjvBKJNSdkvjnnslkdjvbsLKJbvb</p><p align=\"left\">;ossdodhgg;auuhvopiihv;ogieqMy dlfuhg;aoeirbh;oeij</p><p align=\"left\">My dlfuhg;aoeirbh;oeij</p></body></html>">>).

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
    my_test_case,
    two_direction_test, %% Только на 500
    two_direction_test2
  ].

my_test_case(Config) ->
  FilePath = ?config(data_dir, Config) ++ "simple.html",
  {ok, BinaryFile} = file:read_file(FilePath),
  Size1 = byte_size(BinaryFile),
  Size12 = size(zlib:gzip(BinaryFile)),
  Res = bml:encode(BinaryFile),
  Encoded = byte_size(Res),
  ct:pal("Result is ~p", [Res]),
  EncodedZipped = (catch size(zlib:gzip(Res))),
%%  ct:pal("RawSize ~p,~n RawZipped ~p, ~n EncodedSize ~p,~n EncodedZipped ~p", [Size1, Size12, Encoded, EncodedZipped] ),
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

two_direction_test2(Config) ->
  BinaryFile = ?SAMPLE_1,
  ct:pal("~p", [BinaryFile]),
  Res0 = bml:encode(BinaryFile),
  ct:pal("Encoded is ~p", [Res0]),
  Res = decode:decode(1, Res0),
  ct:pal("+++Decoded is ~p", [Res]),
  case Res of
    BinaryFile -> ct:pal("two_direction_test2 is [OK]"),
      {save_config, Config};
    Bad -> ct:pal("two_direction_test2 is [FAIL]"),
      {skip, Bad}
  end,
  Config.