-module(day05).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Ids = lists:map(fun to_seat_id/1, Input),
  Ans = lists:max(Ids),
  io:format("~B~n", [Ans]).

run2(Input) ->
  Ids = lists:sort(lists:map(fun to_seat_id/1, Input)),
  Pairs = lists:zip(Ids, tl(Ids) ++ [0]),
  { value, {X, _} } = lists:search(fun ({ X, Y }) -> X + 2 =:= Y end, Pairs),
  Ans = X + 1,
  io:format("~B~n", [Ans]).

to_seat_id(Line) ->
  { Row, Col } = lists:split(7, tr(Line, "FBLR", "0101")),
  list_to_integer(Row, 2) * 8 + list_to_integer(Col, 2).

tr(Str, Ss, Ts) ->
  lists:foldr(
    fun ({S, T}, Acc) -> lists:concat(string:replace(Acc, [S], [T], all)) end,
    Str,
    lists:zip(Ss, Ts)
  ).

input() ->
  { ok, Device } = file:open("day05-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
