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
  {Tag, _Rest} = tag(Body),
  Tag.

tag(<<I:?TAG_LENGTH, Rest0/bitstring>>) ->
  {Tag, List} = get_tag(I),
  {Attrs, Rest1} = attrs(Rest0, List),
  {Content, Rest} = content(Rest1),
  TagEnding =
    case Content of
      <<>> -> <<"/>">>;
      _ -> << $>, Content/binary, "</", Tag/binary, $>>>
    end,
  {<< $<, Tag/binary, Attrs/binary, TagEnding/binary >>, Rest}.

%% --------------------attributes----------------------------------
attrs(<< Count:?ATTRS_PREFIX_LENGTH, Rest0/bitstring >>, List) ->
  {Attrs, Rest} = read_attrs(Rest0, Count, List),
  {Attrs, Rest}.

read_attrs(Bin, Count, List) ->
  read_attrs(Bin, Count, <<>>, List).
read_attrs(Bin, 0, Acc, _List) ->
  {Acc, Bin};
read_attrs(<< I:?ATTR_KEY_LENGTH, Rest0/bitstring >>, Count, Acc0, List) ->
  Key = get_attr_key(I, List),
  {Value, Rest} = read_attr_value( Key, Rest0 ),
  Separator = <<$\s>>,
  Acc = <<Acc0/binary, Separator/binary , Key/binary, $=, $", Value/binary, $">>,
  read_attrs(Rest, Count - 1, Acc, List).

read_attr_value(DecodedAttr, Bin) ->
  case util:get_value(DecodedAttr, ?ATTR_VALUES) of
    undefined ->
      << ValueSize:?ATTR_VALUE_PREFIX_LENGTH, Rest0/bitstring >> = Bin,
      << Value:ValueSize/binary, Rest/bitstring >> = Rest0,
      {Value, Rest};
    List ->
      << I:?ATTR_VALUE_LENGTH, Rest/bitstring >> = Bin,
      Value = get_attr_value(I, List),
      {Value, Rest}
  end.

%% --------------------Content----------------------------------------
content(<<Count:?CONTENT_PREFIX_SIZE, Rest/bitstring >>) ->
  read_content(Rest, Count, <<>>).

read_content(Bin, 0, Acc) ->
  {Acc, Bin};

read_content(<< ?STRING_MARKER, ContentLength:?STRING_LENGTH_SIZE, Rest0/bitstring>>, Count, Acc) ->
  << ContentString:ContentLength/binary, Rest/bitstring >> = Rest0,
  NewAcc = << Acc/binary, ContentString/binary >>,
  read_content(Rest, Count-1, NewAcc);

read_content(<<_I:?TAG_LENGTH, _Rest0/bitstring>> = Bin, Count, Acc) ->
  {TagContent, Rest} = tag(Bin),
  NewAcc = << Acc/binary, TagContent/binary >>,
  read_content(Rest, Count-1, NewAcc).

%% ---------------------util----------------------------------------
get_tag(Index) ->
  {Tag, List} = lists:nth(Index, ?TAGS_LIST),
  {Tag, ?CORE_ATTRS ++ List}.


get_attr_key(Index, List) -> lists:nth(Index, List).
get_attr_value(Index, List) -> get_attr_key(Index, List).