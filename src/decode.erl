-module(decode).
-author("serg").
-include("bml.hrl").
%% API
-export([
  decode/1,
  decode/2
]).

decode(BML) when is_binary(BML) ->
<< Version:?VERSION_SIZE, Body/binary>> = BML,
  decode(Version, Body).

decode(_Version, Body) ->
  tag(Body).
tag(<<>>) -> <<>>;
tag(<<I:?TAG_LENGTH, Rest0/bitstring>>) ->
%%  ct:pal("Index ~p", [encode:get_index(<<"html">>)]),
%%  ct:pal("Index ~p", [I]),
  {Tag, List} = get_tag(I),
  {Attrs, Rest1} = attrs(Rest0, List),
  {ContentString, Rest} = content(Rest1),
  ct:pal("Tag ~p", [{Tag, Attrs, ContentString}]),
  {{Tag, Attrs, ContentString}, Rest}.

attrs(<< Count:?ATTRS_PREFIX_LENGTH, Rest0/bitstring >>, List) ->
  {Attrs, Rest} = read_attrs(Rest0, Count, List),
  {Attrs, Rest}.

read_attrs(Bin, Count, List) ->
  read_attrs(Bin, Count, <<>>, List).
read_attrs(Bin, 0, Acc, _List) -> {Acc, Bin};
read_attrs(<< I:?ATTR_KEY_LENGTH, Rest0/bitstring >>, Count, Acc0, List) ->
  Key = lists:nth(I, List),
  {Value, Rest} = read_attr_value( Key, Rest0 ),
  Acc = <<Acc0/binary, Key/binary, $=, Value/binary, " " >>,
%%  ct:pal("Value ~p", [Acc]),
  read_attrs(Rest, Count - 1, Acc, List).

read_attr_value(DecodedAttr, Bin) ->
  case util:get_value(DecodedAttr, ?ATTR_VALUES) of
    undefined ->
      << ValueSize:?ATTR_VALUE_PREFIX_LENGTH, Rest0/bitstring >> = Bin,
%%      BitSize = ValueSize * 8,
%%      ct:pal("Bit size ~p", [BitSize]),
      << Value:ValueSize/binary, Rest/bitstring >> = Rest0,
      {Value, Rest};
    List ->
      << I:?ATTR_VALUE_LENGTH, Rest/bitstring >> = Bin,
      Value = lists:nth(I+1, List),
      {Value, Rest}
  end.

content(<< ContentLength:?CONTENT_PREFIX_SIZE, Rest0/bitstring>>) ->
  << ContentString:ContentLength/binary, Rest/bitstring >> = Rest0,
  ct:pal("Cont length ~p String ~p", [ContentLength, ContentString]),
  { ContentString, tag(Rest) }.

get_tag(Index) ->
  {Tag, List} = lists:nth(Index, ?TAGS_LIST),
  {Tag, ?CORE_ATTRS ++ List}.

