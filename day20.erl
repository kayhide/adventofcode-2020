-module(day20).
-export([run/0]).
-import(utils, [echo/1, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day20-input.txt"),
  run1(Input),
  run2(Input).

run1(Input) ->
  [X | Xs] = [ parse(X) || X <- chunks(Input) ],
  [Res | _] = go(
    [[variation({ rotate, 0, flipped, false }, X)]],
    [ Y || X1 <- Xs, Y <- variations(X1) ]
  ),
  Ids = [ id_of(T) || T <- corners(Res) ],
  Ans = lists:foldr(fun(I, Acc) -> I * Acc end, 1, Ids),
  puts(Ans).

run2(Input) ->
  Ans = 2,
  puts(Ans).



go(Current, Tiles) -> go(Current, 0, Tiles).

go(_, 4, _) -> [];

go(Current, _, []) -> [Current];

go(Current = [First | Xs], N, Tiles) ->
  Ms =
    case (0 < length(Xs)) andalso (length(First) < length(hd(Xs))) of
      false ->
        Bottom = hd(First),
        { fun(X) -> [[X], First | Xs] end,
          [ T || T <- Tiles, is_matching_with_bottom_and_top(Bottom, T) ]
        };
      true ->
        Bottom = lists:nth(length(First) + 1, hd(Xs)),
        Left = lists:last(First),
        { fun(X) -> [First ++ [X] | Xs] end,
          [ T || T <- Tiles, is_matching_with_bottom_and_top(Bottom, T), is_matching_with_left_and_right(Left, T) ]
        }
    end,
  case Ms of
    { _, [] } ->
        Next = rotate_whole(Current),
        go(Next, N + 1, Tiles);
    { Add, Ts } ->
        lists:concat([ go(Add(T), N, remove_by_id(T, Tiles)) || T <- Ts ])
  end
  .

corners(Xss) ->
  [hd(hd(Xss)), lists:last(hd(Xss)), lists:last(lists:last(Xss)), hd(lists:last(Xss))].
  
rotate_whole([[] | _]) -> [];
rotate_whole(Whole) ->
  Xs = lists:map(fun hd/1, Whole),
  Xss = lists:map(fun tl/1, Whole),
  [[ rotate_tile(X) || X <- lists:reverse(Xs) ] | rotate_whole(Xss)].

rotate_tile({ { rotate, R, flipped, F }, Entity }) ->
  variation({ rotate, (R + 1) rem 4, flipped, F }, Entity).

remove_by_id(Tile, Tiles) ->
  Id = id_of(Tile),
  [ T || T <- Tiles, id_of(T) /= Id ]. 
  

is_matching_with_bottom_and_top(Bottom, Top) ->
  string:reverse(top_of(Bottom)) =:= bottom_of(Top).

is_matching_with_left_and_right(Left, Right) ->
  string:reverse(right_of(Left)) =:= left_of(Right).


top_of({ Transform, Entity }) -> edge_of(Transform, Entity).

left_of({ { rotate, R, flipped, F }, Entity }) ->
  edge_of({ rotate, (R + 1) rem 4, flipped, F }, Entity).

bottom_of({ { rotate, R, flipped, F }, Entity }) ->
  edge_of({ rotate, (R + 2) rem 4, flipped, F }, Entity).

right_of({ { rotate, R, flipped, F }, Entity }) ->
  edge_of({ rotate, (R + 3) rem 4, flipped, F }, Entity).

id_of({ _, { Id, _, _ } }) -> Id.

variations(Entity = {_, Edges, _}) ->
  [variation({ rotate, X, flipped, F}, Entity)
    || F <- [false, true], X <- lists:seq(0, 3)
  ].

variation(Transform, Entity) -> { Transform, Entity }.

edge_of({ rotate, R, flipped, false }, { _, Edges, _ }) -> lists:nth((4 - R) rem 4 + 1, Edges);
edge_of({ rotate, R, flipped, true }, { _, Edges, _ }) -> string:reverse(lists:nth(R rem 4 + 1, Edges)).

edges({_, Edges, _}) -> Edges.


parse(Chunk) ->
  { Head, Body } = lists:split(1, Chunk),
  Id = list_to_integer(lists:droplast(hd(tl(string:split(Head, " "))))),
  Edges =
    [ hd(Body)
    , [ lists:last(Line) || Line <- Body ]
    , lists:reverse(lists:last(Body))
    , lists:reverse([ hd(Line) || Line <- Body ])
    ],
  { Id, Edges, Body }.

chunks(Input) ->
  F = fun(X, { Chunks, Current }) -> case X of
      "" -> { Chunks ++ [Current], [] };
      _ -> { Chunks, Current ++ [X] }
      end
    end,
  { Chunks, Current } = lists:foldl(F, { [], [] }, Input),
  case Current of
    "" -> Chunks;
    _ -> Chunks ++ [Current]
  end.
