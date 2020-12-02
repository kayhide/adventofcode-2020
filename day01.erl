-module(day01).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  F = fun([X, Y]) -> X + Y == 2020 end,
  Res = lists:search(F, combinations(Input)),
  case Res of
    { value, [X, Y] } -> io:format("~p~n", [X * Y]);
    _ -> io:format("Not found")
  end.

run2(Input) ->
  F = fun([X, Y, Z]) -> X + Y + Z == 2020 end,
  Res = lists:search(F, combinations3(Input)),
  case Res of
    { value, [X, Y, Z] } -> io:format("~p~n", [X * Y * Z]);
    _ -> io:format("Not found")
  end.

input() ->
  { ok, Device } = file:open("day01-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [list_to_integer(string:chomp(Line))| get_all_lines(Device)]
  end.

combinations(List) -> 
  case List of
    [] -> [];
    [X | Xs] ->
      F = fun(Y) -> [X, Y] end,
      lists:map(F, Xs) ++ combinations(Xs)
  end.

combinations3(List) -> 
  case List of
    [] -> [];
    [X| Xs] ->
      F = fun([Y, Z]) -> [X, Y, Z] end,
      lists:map(F, combinations(Xs)) ++ combinations3(Xs)
  end.
