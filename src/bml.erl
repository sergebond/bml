-module(bml).
-author("serg").
-include("bml.hrl").

%% API
-export([
  to_bml/1,
  to_html/1
]).

to_bml(HTML) when is_binary(HTML) ->
  tag_to_bit(HTML, <<>>).
tag_to_bit(<<>>, Acc) -> Acc;
tag_to_bit(<<"<", Tail/binary>>, Acc) ->
  ok;

to_html(BML) when is_binary(BML) ->
  ok.

%%----------------------------------------------------------------------------------------------------------------------
%%                                        PRIVATE
%%----------------------------------------------------------------------------------------------------------------------
search_tag(Tail) ->
  case Res of
    Per -> fuck_r();
    jdnjj -> rkjfk()
  end


get_tag(Index) when is_integer(Index) ->
  lists:nth(Index, ?TAGS).

get_index(Tag) when is_binary(Tag) ->
  get_index(Tag, ?TAGS, 0).
get_index(Tag, [Tag|_], Index) -> Index;
get_index(_, [], _) -> not_found;
get_index(Tag, [_|T], Index) ->
  get_index(Tag, T, Index + 1).
