-module(day21).
-export([run/0]).
-import(utils, [fst/1, bool/3, echo/1, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day21-input.txt"),
  run1(Input),
  run2(Input).

run1(Input) ->
  Foods = [ parse(Line) || Line <- Input ],
  FoodMap = build_map(Foods),
  FoodMap1 = maps:map(fun(_, V) -> intersection(V) end, FoodMap),
  Allergens = lists:concat(maps:values(fix(fun step/1, FoodMap1))),
  Ingrediants = lists:concat( lists:map(fun utils:fst/1, Foods)),
  Res = fix(fun(Xs) -> Xs -- Allergens end, Ingrediants),
  Ans = length(Res),
  puts(Ans).

run2(Input) ->
  Foods = [ parse(Line) || Line <- Input ],
  FoodMap = build_map(Foods),
  FoodMap1 = maps:map(fun(_, V) -> intersection(V) end, FoodMap),
  Xs = fix(fun step/1, FoodMap1),
  Ys = [ X || { _, [X] } <- lists:sort(maps:to_list(Xs)) ],
  Ans = string:join(Ys, ","),
  echo(Ans).

intersection(Xss) ->
  Ys = [ sets:from_list(Xs) || Xs <- Xss ],
 sets:to_list(sets:intersection(Ys)).

step(FoodMap) ->
  Uniqs = [ X || { _, Xs } <- maps:to_list(FoodMap), X <- Xs, length(Xs) =:= 1 ],
  maps:map(fun(_, V) -> bool(V, V -- Uniqs, 1 < length(V)) end, FoodMap).

fix(Step, State) ->
  case Step(State) of
    State -> State;
    Next -> fix(Step, Next)
  end.
  
build_map(Foods) ->
  F = fun({ Vs, Ks }, Acc) ->
    lists:foldl(
      fun(K, Acc1) ->
        maps:update_with(K, fun(Old) -> Old ++ [Vs] end, [Vs], Acc1)
      end, Acc, Ks)
  end,
  lists:foldr(F, #{}, Foods).
 
parse(Line) ->
  Xs = string:split(Line, " ", all),
  { Ys, [_ | Zs] } = lists:splitwith(fun(X) -> X /= "(contains" end, Xs),
  { Ys, [ lists:droplast(Z) || Z <- Zs ] }.
