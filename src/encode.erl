-module(encode).
-author("serg").
-include("bml.hrl").

-define(IS_BLANK(Blank),
  Blank == $\s;
  Blank == $\t;
  Blank == $\n;
  Blank == $\r
).

-define(IS_QUOTE(Quote),
  Quote == $";
  Quote == $'
).

%% API
-export([encode/1]).

%%----------------------------------------------------------------------------------------------------------------------
%%                                        PRIVATE
%%----------------------------------------------------------------------------------------------------------------------
encode(Bin) when is_binary(Bin) ->
  Rest1 = skip_prolog(bstring:trim_left(Bin)),
  Rest2 = skip_doctype(bstring:trim_left(Rest1)),
  {Tag, _Rest} = tag(bstring:trim_left(Rest2)),
  Tag.


skip_prolog(<<"<?xml", Bin/binary>>) ->
  {_, Rest} = bstring:split(Bin, <<"?>">>),
  Rest;
skip_prolog(Bin) ->
  Bin.

skip_doctype(<<"<!", Bin/binary>>) ->
  {_, Rest} = bstring:split(Bin, <<">">>),
  Rest;
skip_doctype(Bin) ->
  Bin.

tag(<<"<", Bin/binary>>) ->
  {TagHeader1, Rest1} = bstring:split(Bin, <<">">>),
  Len = size(TagHeader1)-1,
  case TagHeader1 of
    <<TagHeader:Len/binary, "/">> ->
      {_Tag, EncTag, Attrs} = tag_header(TagHeader),
      {util:bjoin([EncTag, add_attrs_prefix(Attrs)| add_content_prefix([])]), Rest1};
    TagHeader ->
      {Tag, EncTag, Attrs} = tag_header(TagHeader),
      {Content, Rest2} = tag_content(Rest1, Tag),
      {util:bjoin([EncTag, add_attrs_prefix(Attrs) | add_content_prefix(Content)]), Rest2}
  end.

tag_header(Bin) ->
  {Tag, Rest} = bstring:split(Bin, <<" ">>),
  {EncTag, List} = enc_tag(Tag),
  {Tag, EncTag,  tag_attrs(Rest, List)}.

tag_attrs(<<>>, _List) ->
  [];
tag_attrs(<<Blank, Bin/binary>>, List) when ?IS_BLANK(Blank) ->
  tag_attrs(Bin, List);
tag_attrs(Bin, List) ->
  {Key, Value1} = bstring:split(Bin, <<"=">>),
  {Value2, Rest} = attr_value(Value1),
  EncAttrs = enc_attr_key_value(bstring:trim_right(Key), unescape(Value2), List ),
  [EncAttrs | tag_attrs(Rest, List)].

attr_value(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
  attr_value(Bin);
attr_value(<<Quote, Value/binary>>) when ?IS_QUOTE(Quote) ->
  bstring:split(Value, <<Quote>>).

tag_content(<<"<![CDATA[", Bin/binary>>, Tag) ->
  {Text, Rest1} = bstring:split(Bin, <<"]]>">>),
  {Content, Rest2} = tag_content(Rest1, Tag),
  {[Text|Content], Rest2};
tag_content(<<"<!--", Bin/binary>>, Tag) ->
  {_Comment, Rest1} = bstring:split(Bin, <<"-->">>),
  tag_content(Rest1, Tag);
tag_content(<<"</", Bin/binary>>, Tag) ->
  Len = size(Tag),
  <<Tag:Len/binary, Rest1/binary>> = Bin,
  <<">", Rest2/binary>> = bstring:trim_left(Rest1),
  {[], Rest2};
tag_content(<<"<", _/binary>> = Bin, Tag) ->
  {TagData, Rest1} = tag(Bin),
  {Content, Rest2} = tag_content(Rest1, Tag),
  {[TagData|Content], Rest2};
tag_content(<<Blank, Bin/binary>>, Tag) when ?IS_BLANK(Blank) ->
  tag_content(Bin, Tag);
tag_content(Bin, Tag) ->
  {A, _} = binary:match(Bin, <<"<">>),
  <<Text:A/binary, Rest1/binary>> = Bin,
  {Content, Rest2} = tag_content(Rest1, Tag),
  EncTextContent = enc_string(bstring:trim_right(unescape(Text))),
  {[EncTextContent | Content], Rest2}.

unescape(Bin) ->
  case bstring:split(Bin, <<"&">>) of
    {Unescaped, <<>>} ->
      Unescaped;
    {Unescaped, <<"quot;", Rest/binary>>} ->
      <<Unescaped/binary, $", (unescape(Rest))/binary>>;
    {Unescaped, <<"apos;", Rest/binary>>} ->
      <<Unescaped/binary, $', (unescape(Rest))/binary>>;
    {Unescaped, <<"lt;",   Rest/binary>>} ->
      <<Unescaped/binary, $<, (unescape(Rest))/binary>>;
    {Unescaped, <<"gt;",   Rest/binary>>} ->
      <<Unescaped/binary, $>, (unescape(Rest))/binary>>;
    {Unescaped, <<"amp;",  Rest/binary>>} ->
      <<Unescaped/binary, $&, (unescape(Rest))/binary>>
  end.

%% -----------------tag----------------------
-spec enc_tag(binary()) -> {<<_:7>>|not_found, list()}.
enc_tag(Tag) ->
  Res =
    case get_index(Tag) of
      {I, Attrs} when is_integer(I) ->
        {<<I:?TAG_LENGTH>>, Attrs};
      not_found ->
        throw({error, not_found})
    end,
  Res.

%% -----------------attr key----------------------
add_attrs_prefix(Attrs) ->
  AttrsLength = length(Attrs),
  << AttrsLength:?ATTRS_PREFIX_LENGTH,  (util:bjoin(Attrs))/bitstring >>.

enc_attr_key_value( Key, Value, AttrsList ) ->
  EncKey = enc_attr_key(Key, AttrsList),
  ValuesList = get_attr_values_list(Key),
  EncValue = enc_attr_value(Value, ValuesList),
  <<EncKey/bitstring, EncValue/bitstring>>.

enc_attr_key(Key, ValuesList) ->
  Res =
    case get_index(Key, ?CORE_ATTRS ++ ValuesList) of
      I when is_integer(I) ->
        <<I:?ATTR_KEY_LENGTH>>;
      not_found -> not_found
    end,
  Res.

%% -----------------attr value----------------------
get_attr_values_list(Attribute) ->
  util:get_value(Attribute, ?ATTR_VALUES, []).

enc_attr_value(Value, []) ->
  Size = size(Value),
  << Size:?ATTR_VALUE_PREFIX_LENGTH, Value/binary >>;

enc_attr_value(Value, List) ->
  case get_index(Value, List) of
    I when is_integer(I) ->
      <<I:?ATTR_VALUE_LENGTH>>;
    not_found ->
      not_found
  end.

%% -----------------content----------------------
add_content_prefix(Content) when is_list(Content) ->
  Length = length(Content),
  [<<Length:?CONTENT_PREFIX_SIZE>> | Content].

enc_string(Content) when is_binary(Content) ->
  ContentSize = size(Content),
  <<?STRING_CONTENT_PREFIX(ContentSize), Content/binary>>.

%% -----------------utils-------------------------
get_index(Tag) when is_binary(Tag) ->
  get_index(Tag, ?TAGS_LIST, 1).
get_index(Attr, AttrsList) ->
  get_index(Attr, AttrsList, 1).
get_index(Tag, [{Tag, Attrs}|_], Index) ->
  {Index, Attrs};
get_index(Attr, [Attr|_], Index) -> Index ;
get_index(_, [], _) -> not_found;
get_index(Tag, [_|T], Index) ->
  get_index(Tag, T, Index + 1).
