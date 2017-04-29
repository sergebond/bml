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

tag(<<I:?TAG_LENGTH, Rest/bitstring>>) ->
  {Tag, List} = lists:nth(I, ?TAGS_LIST),
  ct:pal("Tag ~p", [Tag]),
  {Tag, attr(Rest, List)}.

attr(<< Count:?ATTRS_PREFIX_LENGTH, Rest0/bitstring >>, List) ->
  {Attrs, Rest} = read_attrs(Rest0, Count, List),
  {Attrs, Rest}.

read_attrs(Bin, Count, List) ->
  read_attrs(Bin, Count, <<>>, List).
read_attrs(Bin, 0, Acc, _List) -> {Acc, Bin};
read_attrs(<< I:?ATTR_KEY_LENGTH, Rest0/bitstring >>, Count, Acc0, List) ->
  Key = lists:nth(I, List),
  {Value, Rest} = read_attr_value( Key, Rest0 ),
  Acc = <<Acc0/binary, Key/binary, $=, Value/binary, " " >>,
  read_attrs(Rest, Count-1, Acc, List).

read_attr_value(DecodedAttr, Bin) ->
  case util:get_value(DecodedAttr, ?ATTR_VALUES) of
    undefined ->
      << ValueSize:?ATTR_VALUE_PREFIX_LENGTH, Rest0/bitstring >> = Bin,
      BitSize = ValueSize * 8,
      << Value:BitSize, Rest/bitstring >> = Rest0,
      {Value, Rest};
    List ->
      << I:?ATTR_VALUE_LENGTH, Rest/bitstring >> = Bin,
      Value = lists:nth(I, List),
      {Value, Rest}
  end.