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
  G = digraph:new(),
  Vs = [ digraph:add_vertex(G, N) || N <- Ns1 ],
  [ digraph:add_edge(G, V1, V2) || { V1, V2 } <- lists:zip(Ns1, tl(Ns1) ++ [hd(Ns1)]) ],
  { G, hd(Vs), Max1 }.
  
move({ G, Cur, Max }) ->
  [N1] = digraph:out_neighbours(G, Cur),
  [N2] = digraph:out_neighbours(G, N1),
  [N3] = digraph:out_neighbours(G, N2),
  [N4] = digraph:out_neighbours(G, N3),
  Dst = find_destination(Cur - 1, Max, [N1, N2, N3]),
  % puts(take(9, Cur, { G, Cur, Max })),
  % puts({ current, Cur, destination, Dst, taking, [N1, N2, N3] }),
  outsert(G, N1, N3),
  insert_after(G, Dst, N1, N3),
  { G, N4, Max }.

outsert(G, V0, V1) ->
  [Prev] = digraph:in_neighbours(G, V0),
  [Next] = digraph:out_neighbours(G, V1),
  digraph:del_edges(G, digraph:in_edges(G, V0)),
  digraph:del_edges(G, digraph:out_edges(G, V1)),
  digraph:add_edge(G, Prev, Next).

insert_after(G, Prev, V0, V1) ->
  [Next] = digraph:out_neighbours(G, Prev),
  digraph:del_edges(G, digraph:out_edges(G, Prev)),
  digraph:del_edges(G, digraph:in_edges(G, Next)),
  digraph:add_edge(G, Prev, V0),
  digraph:add_edge(G, V1, Next).
  
take(Count, At, { G, _, _ }) -> take(Count, At, G);
take(Count, At, G) ->
  { _, Ns } =
  lists:foldl(
    fun(_, { V, Vs }) ->
      [Next] = digraph:out_neighbours(G, V) ,
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
