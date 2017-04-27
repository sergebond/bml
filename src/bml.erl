-module(bml).
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
-export([
  to_bml/1,
  to_html/1
]).

%% API
%%-export([decode_document/1]).
-export([decode/1]).



to_bml(HTML) when is_binary(HTML) ->
  tag_to_bit(HTML, <<>>).
tag_to_bit(<<>>, Acc) -> Acc;
tag_to_bit(<<"<", Tail/binary>>, Acc) ->
  ok.

to_html(BML) when is_binary(BML) ->
  ok.

%%----------------------------------------------------------------------------------------------------------------------
%%                                        PRIVATE
%%----------------------------------------------------------------------------------------------------------------------
-spec encode_tag(binary()) -> {<<_:7>>|not_found, list()}.
encode_tag(Tag) ->
  Res =
    case get_index(Tag) of
      {I, Attrs} when is_integer(I) ->
        {<<I:?TAG_LENGTH>>, Attrs};
      non_found ->
        throw({error, not_found})
    end,
  ct:pal("Index~p", [Res]),
  Res.

-spec encode_attr(binary(), list()) -> <<_:3>>|not_found.
encode_attr(Attr, AttrsList) ->
  Res =
    case get_index(Attr, ?CORE_ATTRS ++ AttrsList) of
      I when is_integer(I) ->
        <<I:?ATTR_LENGTH>>;
      not_found -> not_found
%%        throw({error, not_found})
    end,
  ct:pal("Index~p", [Res]),
  Res.

encode_attr_values(Attr, AttrsList) ->

  Res =
    case get_index(Attr, ?CORE_ATTRS ++ AttrsList) of
      I when is_integer(I) ->
        <<I:?ATTR_VALUES_LENGTH>>;
      not_found -> not_found
%%        throw({error, not_found})
    end,
  ct:pal("Index~p", [Res]),
  Res.

get_tag(Index) when is_integer(Index) ->
  lists:nth(Index, ?TAGS_LIST).

get_index(Tag) when is_binary(Tag) ->
  get_index(Tag, ?TAGS_LIST, 0).
get_index(Attr, AttrsList) ->
  get_index(Attr, AttrsList, 0).
get_index(Tag, [{Tag, Attrs}|_], Index) ->
  {Index, Attrs};
get_index(Attr, [Attr|_], Index) -> Index;
get_index(_, [], _) -> not_found;
get_index(Tag, [_|T], Index) ->
  get_index(Tag, T, Index + 1).

%%----------------------------------------------------------------------------------------------------------------------
%%                                        PRIVATE
%%----------------------------------------------------------------------------------------------------------------------


%% API
%%decode_document(Bin) when is_binary(Bin) ->
%%  {Version, Encoding, Rest1} = prolog(bstring:trim_left(Bin)),
%%  Rest2 = skip_doctype(bstring:trim_left(Bin)),
%%  {Tag, _Rest} = tag(bstring:trim_left(Rest2)),
%%  {xml, Tag}.
%%
decode(Bin) when is_binary(Bin) ->
  Rest1 = skip_prolog(bstring:trim_left(Bin)),
  Rest2 = skip_doctype(bstring:trim_left(Rest1)),
  {Tag, _Rest} = tag(bstring:trim_left(Rest2)),
  Tag.
%%
%%%% internal
%%prolog(<<"<?xml", Bin/binary>>) ->
%%  {Prolog, Rest} = bstring:split(Bin, <<"?>">>),
%%  Attrs = tag_attrs(Prolog),
%%  {get_version(Attrs), get_encoding(Attrs), Rest}.
%%
%%get_version(Attrs) ->
%%  case get_value(<<"version">>, Attrs) of
%%    <<"1.0">> -> '1.0';
%%    <<"1.1">> -> '1.1'
%%  end.
%%
%%get_encoding(Attrs) ->
%%  Encoding = get_value(<<"encoding">>, Attrs),
%%  case bstring:to_lower(Encoding) of
%%    <<"iso-8859-1">>    -> latin1;
%%    <<"iso_8859_1">>    -> latin1;
%%    <<"iso_8859-1">>    -> latin1;
%%    <<"iso8859-1">>     -> latin1;
%%    <<"utf-8">>         -> utf8;
%%    <<"utf_8">>         -> utf8;
%%    <<"utf8">>          -> utf8;
%%    <<>>                -> undefined;
%%    _                   -> unknown
%%  end.

get_value(Key, List) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Value} -> Value;
    false -> <<>>
  end.

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
      {Tag, EncodedTag, Attrs} = tag_header(TagHeader),
      {{EncodedTag, Attrs,[]}, Rest1};
    TagHeader ->
      {Tag, EncodedTag, Attrs} = tag_header(TagHeader),
      {Content, Rest2} = tag_content(Rest1, Tag),
      {{EncodedTag, Attrs, Content}, Rest2}
  end.

tag_header(Bin) ->
  {Tag, Rest} = bstring:split(Bin, <<" ">>),
  {Encoded, AttrsList} = encode_tag(Tag),
  {Tag, Encoded, tag_attrs(Rest, AttrsList)}.

tag_attrs(<<>>, _) ->
  [];
tag_attrs(<<Blank, Bin/binary>>, AttrsList) when ?IS_BLANK(Blank) ->
  tag_attrs(Bin, AttrsList);
tag_attrs(Bin, AttrsList) ->
  {Key, Value1} = bstring:split(Bin, <<"=">>),
  {Value2, Rest} = attr_value(Value1),
  Encoded = encode_attr(bstring:trim_right(Key), AttrsList),
  [{Encoded, unescape(Value2)}|tag_attrs(Rest, AttrsList)].

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
  ct:pal("Badm ~p, tag ~P", [Tag, Bin, 20]),
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
  {[bstring:trim_right(unescape(Text))|Content], Rest2}.

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
