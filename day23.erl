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
  iterate(100, fun move/1, Ns),
  [_ | Xs] = take(9, 1, Ns),
  Ans = lists:concat([ integer_to_list(I) || I <- Xs ]),
  echo(Ans).

run2(Input) ->
  Ns = construct(1000 * 1000, Input),
  iterate(10 * 1000 * 1000, fun move/1, Ns),
  [_, X, Y | _] = take(3, 1, Ns),
  puts([X, Y]),
  Ans = X * Y,
  puts(Ans).


construct(Input) -> construct(nil, Input).

construct(Max, Input) ->
  Ns = [ list_to_integer([C]) || Line <- Input, C <- Line ],
  { Ns1, Max1 } =
    case Max of
      nil -> { Ns, lists:max(Ns) };
      _ -> { Ns ++ lists:seq(lists:max(Ns) + 1, Max), Max }
    end,
  Set = ets:new(cups, [set, private]),
  [ ets:insert(Set, {V1, V2}) || { V1, V2 } <- lists:zip(Ns1, tl(Ns1) ++ [hd(Ns1)]) ],
  { Set, hd(Ns), Max1 }.

move({ T, Cur, Max }) ->
  [{_, N1}] = ets:lookup(T, Cur),
  [{_, N2}] = ets:lookup(T, N1),
  [{_, N3}] = ets:lookup(T, N2),
  [{_, N4}] = ets:lookup(T, N3),
  Dst = find_destination(Cur - 1, Max, [N1, N2, N3]),
  % puts(take(9, Cur, { T, Cur, Max })),
  % puts({ current, Cur, destination, Dst, taking, [N1, N2, N3] }),
  outsert(T, Cur, N4),
  insert_after(T, Dst, N1, N3),
  { T, N4, Max }.

outsert(T, V0, V1) ->
  ets:insert(T, {V0, V1}).

insert_after(T, Prev, V0, V1) ->
  [{_, Next}] = ets:lookup(T, Prev),
  ets:insert(T, {Prev, V0}),
  ets:insert(T, {V1, Next}).
  
take(Count, At, { T, _, _ }) -> take(Count, At, T);
take(Count, At, T) ->
  { _, Ns } =
  lists:foldl(
    fun(_, { V, Vs }) ->
      [{_, Next}] = ets:lookup(T, V) ,
      { Next, Vs ++ [V] }
    end, { At, [] }, lists:seq(1, Count)),
  Ns.
  

find_destination(0, Max, Out) -> find_destination(Max, Max, Out);
find_destination(N, Max, Out) ->
  case lists:member(N, Out) of
    false -> N;
    true -> find_destination(N - 1, Max, Out)
  end.

iterate(0, _, X) -> X;
iterate(N, F, X) ->
  case N rem 100000 of
    0 -> puts(N);
    _ -> nop
  end,
  iterate(N - 1, F, F(X)).
