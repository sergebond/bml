-module(util).
-author("serg").

%% API
-export([
  get_value/2,
  get_value/3,
  bjoin/1
]).

get_value(Key, Value) ->
  get_value(Key, Value, undefined).
get_value(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Value} -> Value;
    false -> Default
  end.

bjoin([]) -> <<>>;
bjoin([H|T]) ->
  << H/bitstring, (bjoin(T))/bitstring >>.

