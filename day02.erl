-module(day02).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Xs = lists:map(fun(X) -> parse(X) end, Input),
  F = fun({Min, Max, C, Body}) ->
    Cs = lists:filter(fun(X) -> X == C end, Body),
    L = string:length(Cs),
    ((Min =< L) and (L =< Max))
  end,
  L = length(lists:filter(F, Xs)),
  io:format("~B~n", [L]).

run2(Input) ->
  Xs = lists:map(fun(X) -> parse(X) end, Input),
  F = fun({Min, Max, C, Body}) ->
    Cs = [lists:nth(Min, Body), lists:nth(Max, Body)],
    L = string:length(lists:filter(fun(X) -> X == C end, Cs)),
    L == 1
  end,
  L = length(lists:filter(F, Xs)),
  io:format("~B~n", [L]).

parse(Line) ->
  [MinMax, [C |_], Body | _] = string:split(Line, " ", all),
  [Min, Max | _] = lists:map(fun(X) -> list_to_integer(X) end, string:split(MinMax, "-")),
  { Min, Max, C, Body}.
  
input() ->
  { ok, Device } = file:open("day02-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
