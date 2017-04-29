-module(bml).
-author("serg").
-include("bml.hrl").

%% API
-export([
  encode/1
%%  to_html/1
]).

encode(HTML) when is_binary(HTML) ->
  encode:encode(HTML).