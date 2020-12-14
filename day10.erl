-module(day10).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Xs = [0] ++ lists:sort(lists:map(fun list_to_integer/1, Input)),
  Ys = lists:zipwith(fun(S, L) -> L - S end, lists:droplast(Xs), tl(Xs)),
  F = fun(N, { One, Three }) -> case N of
      1 -> { One + 1, Three };
      3 -> { One, Three + 1 }
      end
    end,
  { One, Three } = lists:foldr(F, { 0, 0 }, Ys),
  Ans = One * (Three + 1),
  io:format("~B~n", [Ans]).

run2(Input) ->
  Xs = [0] ++ lists:sort(lists:map(fun list_to_integer/1, Input)),
  Ys = lists:zipwith(fun(S, L) -> L - S end, lists:droplast(Xs), tl(Xs)),
  Anses = lists:map(fun count_arrangements/1, group(Ys)),
  Ans = lists:foldr(fun(X, Acc) -> X * Acc end, 1, Anses),
  io:format("~B~n", [Ans]).

count_arrangements(Xs) ->
  case Xs of
    [2, 3 | Ys] -> count_arrangements(Ys);
    [2, 2 | Ys] -> count_arrangements([2 | Ys]);
    [2, 1 | Ys] ->
     count_arrangements([1 | Ys]) +
     count_arrangements(Ys);
    [1, 3 | Ys] -> count_arrangements(Ys);
    [1, 2 | Ys] ->
     count_arrangements([2 | Ys]);
    [1, 1 | Ys] ->
     count_arrangements([1 | Ys]) +
     count_arrangements([2 | Ys]);
    [3| Ys] -> count_arrangements(Ys);
    [_] -> 1;
    [] -> 1;
    _ -> 0
  end.

group(Xs) ->
  F = fun(X, { Acc, Cur }) ->
    case Cur of
      [] -> { Acc, [X] };
      [X | _] ->  { Acc, [X | Cur] };
      _ -> { [Cur | Acc], [X] }
      end
    end,
  { Acc, Cur } = lists:foldr(F, { [], [] }, Xs),
  [Cur | Acc].
  

input() ->
  { ok, Device } = file:open("day10-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
