-module(day03).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Width = string:length(lists:nth(1, Input)),
  Step = fun(X) -> { (X + 3) rem Width, 1 } end,
  Ans = go(Input, Step),
  io:format("~B~n", [Ans]).

run2(Input) ->
  Width = string:length(lists:nth(1, Input)),
  Xs = [
    go(Input, fun(X) -> { (X + 1) rem Width, 1 } end),
    go(Input, fun(X) -> { (X + 3) rem Width, 1 } end),
    go(Input, fun(X) -> { (X + 5) rem Width, 1 } end),
    go(Input, fun(X) -> { (X + 7) rem Width, 1 } end),
    go(Input, fun(X) -> { (X + 1) rem Width, 2 } end)
  ],
  Ans = lists:foldr(fun (X, Acc) -> X * Acc end, 1, Xs),
  io:format("~B~n", [Ans]).

go(Lines, Step) ->
  go(Lines, Step, {0, 0}).

go(Lines, Step, {X, Y}) ->
  case {Lines, Y} of
    {[], _} -> 0;
    {[Line| _], 0} -> case lists:nth(X + 1, Line) of
      $# -> 1 + go(Lines, Step, Step(X));
      _ -> go(Lines, Step, Step(X))
    end;
    {[_| Lines_], _} -> go(Lines_, Step, {X, Y - 1})
  end.  

input() ->
  { ok, Device } = file:open("day03-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
