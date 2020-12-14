-module(day09).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  { Pre, Xs } = lists:split(25, lists:map(fun parse/1, Input)),
  Ans = go(Pre, Xs),
  io:format("~B~n", [Ans]).

run2(Input) ->
  { Pre, Xs } = lists:split(25, lists:map(fun parse/1, Input)),
  N = go(Pre, Xs),
  Ys = find_contiguous(N, Xs),
  Ans = hd(Ys) + lists:last(Ys),
  io:format("~B~n", [Ans]).

find_contiguous(N, Xs) ->
  Pred = fun(Ys) -> lists:sum(Ys) =< N end,
  Ys = lists:last(lists:takewhile(Pred, inits(Xs))),
  
  case lists:sum(Ys) of
    N -> Ys;
    _ -> find_contiguous(N, tl(Xs))
  end.
  
inits([]) -> [[]];
inits([X | Xs]) ->
  [[X] | lists:map(fun(Y) -> [X | Y] end, inits(Xs))].
  

go(Pre, [X | Xs]) ->
  F = fun({Y, Z}) -> Y + Z =:= X end,
  case lists:search(F, combinations(Pre)) of
    { value, _ } -> go(tl(Pre) ++ [X], Xs);
    _ -> X
  end.

combinations(List) -> 
  case List of
    [] -> [];
    [X | Xs] ->
      F = fun(Y) -> {X, Y} end,
      lists:map(F, Xs) ++ combinations(Xs)
  end.

parse(Line) -> list_to_integer(Line).

input() ->
  { ok, Device } = file:open("day09-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
