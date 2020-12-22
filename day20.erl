-module(day20).
-export([run/0]).
-import(utils, [count/2, update_at/3, echo/1, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day20-input.txt"),
  run1(Input),
  run2(Input),
  puts().

run1(Input) ->
  Tiless = assemble(Input),
  % display(Tiless),
  Ids = [ id_of(T) || T <- corners(Tiless) ],
  Ans = lists:foldr(fun(I, Acc) -> I * Acc end, 1, Ids),
  puts(Ans).

run2(Input) ->
  Tiless = assemble(Input),
  Image = construct(Tiless),
  Image1 = lists:reverse(Image),
  Images =
    [ Image
    , rotate(Image)
    , rotate(rotate(Image))
    , rotate(rotate(rotate(Image)))
    , Image1
    , rotate(Image1)
    , rotate(rotate(Image1))
    , rotate(rotate(rotate(Image1)))
    ],

  Monster =
    [ "                  # "
    , "#    ##    ##    ###"
    , " #  #  #  #  #  #   "
    ],

  Pss = [ { Img, look_for(Monster, Img) } || Img <- Images ], 
  [{ Img, Ps }] = lists:filter(fun({ _, Ps }) -> 0 < length(Ps) end, Pss),

  % Img1 = lists:foldr(fun(P, Acc) -> overlay(P, Monster, Acc) end, Img, Ps),
  % [ echo(Line) || Line <- Img ],
  % [ echo(Line) || Line <- Img1 ],

  Ans = count($#, lists:concat(Image)) - length(Ps) * count($#, lists:concat(Monster)),
  puts(Ans).

overlay({ X, Y }, Monster, Image) ->
  MonsterPoss = poss_from_image(Monster),
  F = fun({ I, J }, Acc) -> update_at(Y + J, update_at(X + I, $O, lists:nth(Y + J + 1, Acc)), Acc) end,
  lists:foldr(F, Image, MonsterPoss).

offsets(Inner, Outer) ->
  { InnerW, InnerH } = { length(hd(Inner)), length(Inner) },
  { OuterW, OuterH } = { length(hd(Outer)), length(Outer) },
  [ { X, Y } || Y <- lists:seq(0, OuterH - InnerH), X <- lists:seq(0, OuterW - InnerW) ].

poss_from_image(Image) ->
  F =
    fun(C, { I, Poss }) ->
      case C of
        $# -> { I + 1, Poss ++ [I] };
        _ -> { I + 1, Poss }
      end
    end,
  [ { X, Y }
    || { Line, Y } <- lists:zip(Image, lists:seq(0, length(Image) - 1))
    ,  X <- case lists:foldl(F, { 0, [] }, Line) of { _, Xs } -> Xs end
  ].

look_for(Monster, Image) ->
  MonsterPoss = poss_from_image(Monster),
  Offsets = offsets(Monster, Image),
  lists:filter(fun(Offset) -> is_matching_at(Offset, MonsterPoss, Image) end, Offsets).

is_matching_at({ Dx, Dy }, Poss, Image) ->
  Pred = fun({ X, Y }) -> lists:nth(X + Dx + 1, lists:nth(Y + Dy + 1, Image)) =:= $# end,
  lists:all(Pred, Poss).

assemble(Input) ->
  [X | Xs] = [ parse(X) || X <- chunks(Input) ],
  [Res | _] = go(
    [[variation({ rotate, 0, flipped, false }, X)]],
    [ Y || X1 <- Xs, Y <- variations(X1) ]
  ),
  Res.
  
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

display(Rows) ->
  [ [ echo(lists:join(" ", Xs)) || Xs <- (Xss ++ [[]]) ] || Xss <- bake_whole(Rows) ].

construct(Rows) -> glue([ [ glue(Xs) || Xs <- Xss ] || Xss <- bake_whole(Rows) ]).

glue(Xs) -> lists:concat([ lists:droplast(tl(X)) || X <- Xs ]).

transpose([[] | _]) -> [];
transpose(Xss) -> [ [ hd(Xs) || Xs <- Xss ] | transpose([ tl(Xs) || Xs <- Xss ]) ].

rotate(Xss) -> [ lists:reverse(Xs) || Xs <- transpose(Xss) ].

bake_whole(Rows) -> [ transpose([ bake(Tile) || Tile <- Row ]) || Row <- Rows ].

bake({ Transform, { _, _, Body } }) -> bake(Transform, Body).
bake({ rotate, 0, flipped, false }, Body) -> Body;
bake({ rotate, R, flipped, false }, Body) ->
  bake({ rotate, R - 1, flipped, false }, [ string:reverse(Line) || Line <- transpose(Body) ]);
bake({ rotate, R, flipped, true }, Body) ->
  bake({ rotate, R, flipped, false }, lists:reverse(Body)).

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

variations(Entity) ->
  [variation({ rotate, X, flipped, F}, Entity)
    || F <- [false, true], X <- lists:seq(0, 3)
  ].

variation(Transform, Entity) -> { Transform, Entity }.

edge_of({ rotate, R, flipped, false }, { _, Edges, _ }) -> lists:nth((4 - R) rem 4 + 1, Edges);
edge_of({ rotate, R, flipped, true }, { _, [Top, Right, Bottom, Left], _ }) ->
  string:reverse(lists:nth((4 - R) rem 4 + 1, [Bottom, Right, Top, Left])).


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
