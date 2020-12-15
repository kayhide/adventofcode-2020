-module(day12).
-export([run/0]).
-import(utils, [puts/1]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Insts = lists:map(fun parse/1, Input),
  { { X, Y }, _ } = lists:foldl(fun exec1/2, { { 0, 0 }, { 1, 0 } }, Insts),
  Ans = abs(X) + abs(Y),
  puts(Ans).

exec1(Inst, State = { Pos = { X, Y }, Dir = { Dx, Dy } }) ->
  case Inst of
    { north, V } -> { { X, Y - V }, Dir };
    { south, V } -> { { X, Y + V }, Dir };
    { east, V } -> { { X + V, Y }, Dir };
    { west, V } -> { { X - V, Y }, Dir };
    { left, 90 } -> { Pos, turn_left(Dir) };
    { left, V } -> exec1({ left, V - 90 }, exec1({ left, 90 }, State));
    { right, 90 } -> { Pos, turn_right(Dir) };
    { right, Deg } -> exec1({ right, Deg - 90 }, exec1({ right, 90 }, State));
    { forward, V } -> { { X + Dx * V, Y + Dy * V }, Dir }
  end.

run2(Input) ->
  Insts = lists:map(fun parse/1, Input),
  { { X, Y }, _ } = lists:foldl(fun exec2/2, { { 0, 0 }, { 10, -1 } }, Insts),
  Ans = abs(X) + abs(Y),
  puts(Ans).

exec2(Inst, State = { Pos = { X, Y }, Dir = { Dx, Dy } }) ->
  case Inst of
    { north, V } -> { Pos, { Dx, Dy - V } };
    { south, V } -> { Pos, { Dx, Dy + V } };
    { east, V } -> { Pos, { Dx + V, Dy } };
    { west, V } -> { Pos, { Dx - V, Dy } };
    { left, 90 } -> { Pos, turn_left(Dir) };
    { left, V } -> exec2({ left, V - 90 }, exec2({ left, 90 }, State));
    { right, 90 } -> { Pos, turn_right(Dir) };
    { right, Deg } -> exec2({ right, Deg - 90 }, exec2({ right, 90 }, State));
    { forward, V } -> { { X + Dx * V, Y + Dy * V }, Dir }
  end.

turn_left({ Dx, Dy }) -> { Dy, - Dx }.
turn_right({ Dx, Dy }) -> { - Dy, Dx }.

parse([K | V]) ->
  I = list_to_integer(V),
  case K of
    $N -> { north, I };
    $S -> { south, I };
    $E -> { east, I };
    $W -> { west, I };
    $L -> { left, I };
    $R -> { right, I };
    $F -> { forward, I }
  end.

input() ->
  { ok, Device } = file:open("day12-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
