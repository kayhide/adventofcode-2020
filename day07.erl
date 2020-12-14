-module(day07).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Rules = maps:from_list(lists:map(fun parse/1, Input)),
  Ans = sets:size(containers(Rules, "shiny gold")),
  io:format("~B~n", [Ans]).

run2(Input) ->
  Rules = maps:from_list(lists:map(fun parse/1, Input)),
  Ans = count_children(Rules, "shiny gold"),
  io:format("~B~n", [Ans]).


containers(Rules, Col) ->
  Pred = fun(_, Child) -> maps:is_key(Col, Child) end,
  Parents = sets:from_list(maps:keys(maps:filter(Pred, Rules))),
  sets:union([Parents | [containers(Rules, X) || X <- sets:to_list(Parents)]]).

count_children(Rules, Col) ->
  F = fun(K, N) -> N * (1 + count_children(Rules, K)) end,
  lists:sum(maps:values(maps:map(F, maps:get(Col, Rules)))).
  
parse(Line) ->
  [Parent, Xs] = string:split(Line, " bags contain "),
  F = fun(X) ->
      [N | Words] = string:split(X, " ", all),
      V = case N of
        "no" -> 0;
        _ -> list_to_integer(N)
      end,
      K = string:join(lists:droplast(Words), " "),
      { K, V }
    end,
  Children = maps:from_list(
    lists:filter(fun({ _, V }) -> 0 < V end,
    lists:map(F, string:split(Xs, ", ", all)))),
  { Parent, Children }.

input() ->
  { ok, Device } = file:open("day07-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
