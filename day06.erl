-module(day06).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Groups = chunks(Input),
  F = fun(Xs, Acc) -> sets:size(sets:union([sets:from_list(X) || X <- Xs])) + Acc end,
  Ans = lists:foldr(F, 0, Groups),
  io:format("~B~n", [Ans]).

run2(Input) ->
  Groups = chunks(Input),
  F = fun(Xs, Acc) -> sets:size(sets:intersection([sets:from_list(X) || X <- Xs])) + Acc end,
  Ans = lists:foldr(F, 0, Groups),
  io:format("~B~n", [Ans]).


chunks(Input) ->
  F = fun(X, { Chunks, Current }) -> case X of
      "" -> { Chunks ++ [Current], [] };
      _ -> { Chunks, Current ++ [X] }
      end
    end,
  { Chunks, Current } = lists:foldr(F, { [], [] }, Input),
  case Current of
    "" -> Chunks;
    _ -> Chunks ++ [Current]
  end.

input() ->
  { ok, Device } = file:open("day06-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
