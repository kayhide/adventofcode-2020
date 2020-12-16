-module(day15).
-export([run/0]).
-import(utils, [puts/1]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Xs = [ list_to_integer(X) || X <- string:split(Input, ",", all) ],
  { Init, [Last] } = lists:split(length(Xs) - 1, Xs),
  Nums = maps:from_list(lists:zip(Init, lists:seq(0, length(Init) - 1))),
  State = { Last, length(Init), Nums },
  
  { Ans, _, _ } = go(2020 - 1, fun next/1, State),
  puts(Ans).

run2(Input) ->
  Xs = [ list_to_integer(X) || X <- string:split(Input, ",", all) ],
  { Init, [Last] } = lists:split(length(Xs) - 1, Xs),
  Nums = maps:from_list(lists:zip(Init, lists:seq(0, length(Init) - 1))),
  State = { Last, length(Init), Nums },
  
  { Ans, _, _ } = go(30000000 - 1, fun next/1, State),
  puts(Ans).


go(N, F, State) ->
  case F(State) of
    Next = { _, N, _ } -> Next;
    Next -> go(N, F, Next)
  end.

next({ Last, I, Nums }) ->
  % puts({ I, Last }),
  case maps:get(Last, Nums, nil) of
    nil -> { 0, I + 1, maps:put(Last, I, Nums) };
    X -> V = I - X, { V, I + 1, maps:put(Last, I, Nums) }
  end.
  
input() ->
  { ok, Device } = file:open("day15-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
