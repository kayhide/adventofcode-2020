-module(day23).
-export([run/0]).
-import(utils, [echo/1, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day23-input.txt"),
  % Input = ["389125467"],
  run1(Input),
  run2(Input).

run1(Input) ->
  Ns = construct(Input),
  Ns100 = iterate(100, fun move/1, Ns),
  { Xs, [1 | Ys] } = lists:splitwith(fun(I) -> I /= 1 end, Ns100),
  Ans = lists:concat([ integer_to_list(I) || I <- Ys ++ Xs ]),
  echo(Ans).

run2(Input) ->
  Ans = 2,
  puts(Ans).


construct(Input) -> construct(nil, Input).

construct(Max, Input) ->
  Ns = [ list_to_integer([C]) || Line <- Input, C <- Line ],
  Ns1 =
    case Max of
      nil -> Ns;
      _ -> Ns ++ lists:seq(lists:max(Ns), Max)
    end,
  Ns1.
  
move([C, N1, N2, N3 | Ns]) ->
  C1 = find_next(C - 1, lists:max(Ns), [N1, N2, N3]),
  { Pre, [C1 | Post] } = lists:splitwith(fun(I) -> I /= C1 end, Ns),
  Pre ++ [C1, N1, N2, N3] ++ Post ++ [C].
  

find_next(0, Max, Out) -> find_next(Max, Max, Out);
find_next(N, Max, Out) ->
  case lists:member(N, Out) of
    false -> N;
    true -> find_next(N - 1, Max, Out)
  end.

iterate(0, _, X) -> X;
iterate(N, F, X) ->
  iterate(N - 1, F, F(X)).
  
