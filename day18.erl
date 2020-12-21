-module(day18).
-export([run/0]).
-import(utils, [echo/1, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day18-input.txt"),
  run1(Input),
  run2(Input).


run1(Input) ->
  % Xs = [ eval(parse1("1 + 2 * 3 + 4 * 5 + 6")) ],
  % Xs = [ eval(parse1("1 + (2 * 3) + (4 * (5 + 6))")) ],
  Xs = [ eval(parse1(Line)) || Line <- Input ],
  Ans = lists:sum(Xs),
  puts(Ans).

run2(Input) ->
  % Xs = [ eval(parse2("1 + 2 * 3 + 4 * 5 + 6")) ],
  % Xs = [ eval(parse2("1 + (2 * 3) + (4 * (5 + 6))")) ],
  % Xs = [ eval(parse2("5 + (8 * 3 + 9 + 3 * 4 * 3)")) ],
  Xs = [ eval(parse2(Line)) || Line <- Input ],
  Ans = lists:sum(Xs),
  puts(Ans).

eval({ add, X, Y }) -> eval(X) + eval(Y);
eval({ multiply, X, Y }) -> eval(X) * eval(Y);
eval(X) -> X.


parse1({ Left, Str }) ->
  case Str of
    "" -> Left;
    [ $+ | Str1 ] ->
      { Right, Str2 } = parse_term(fun parse1/1, Str1),
      parse1({ { add, Left, Right }, Str2 });
    [ $* | Str1 ] ->
      { Right, Str2 } = parse_term(fun parse1/1, Str1),
      parse1({ { multiply, Left, Right }, Str2 });
    [ $  | Str1 ] -> parse1({ Left, Str1 });
    _ -> { Left, Str }
  end;

parse1(Str) ->
  { Left, Str1 } = parse_term(fun parse1/1, Str),
  parse1({ Left, Str1 }).


parse2({ Left, Str }) ->
  case Str of
    "" -> Left;
    [ $* | Str1 ] ->
      { X, Str2 } = parse_term(fun parse2/1, Str1),
      { Right, Str3 } = parse_plus_factor(fun parse2/1, X, Str2),
      parse2({ { multiply, Left, Right }, Str3 });
    [ $  | Str1 ] -> parse2({ Left, Str1 });
    _ -> { Left, Str }
  end;
  
parse2(Str) ->
  { X, Str1 } = parse_term(fun parse2/1, Str),
  { Left, Str2 } = parse_plus_factor(fun parse2/1, X, Str1),
  parse2({ Left, Str2 }).

parse_plus_factor(P, Left, Str) ->
  case Str of
    [ $+ | Str1 ] ->
      { Right, Str2 } = parse_term(P, Str1),
      parse_plus_factor(P, { add, Left, Right }, Str2);
    [ $  | Str1 ] -> parse_plus_factor(P, Left, Str1);
    _ -> { Left, Str }
  end.

parse_term(P, Str) ->
  case Str of
    [ $( | Str1 ] ->
      { Term, Str2 } = P(Str1),
      { _, Str3 } = parse_char($), Str2),
      { Term, Str3 };
    [ $  | Str1 ] -> parse_term(P, Str1);
    _ ->
      { Num, Str1 } = lists:splitwith(fun(C) -> ($0 =< C) andalso (C =< $9) end, Str),
      { list_to_integer(Num), Str1 }
  end.

parse_char(C, [C | Str]) -> { C, Str }.
