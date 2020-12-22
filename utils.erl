-module(utils).
-compile(export_all).

% Tuple
fst({ F, _ }) -> F.
snd({ _, S }) -> S.

% Boolean
bool(F, _T, false) -> F;
bool(_F, T, true) -> T.

% List
chunks(_N, []) -> [];
chunks(N, Xs) ->
  case lists:split(N, Xs) of
    { Chunk, Ys } -> [Chunk | chunks(N, Ys)]
  end.

count(X, Xs) ->
  F = fun(Y, Acc) ->
    case Y of
      X -> Acc + 1;
      _ -> Acc
    end
  end,
  lists:foldr(F, 0, Xs).

update_at(0, Y, [_ | Xs]) -> [Y | Xs];
update_at(I, Y, [X | Xs]) -> [X | update_at(I - 1, Y, Xs)].

% IO
puts() -> io:format("~n").
puts(X) -> io:format("~p~n", [X]).

echo(X) -> io:format("~s~n", [X]).

input(Filename) ->
  { ok, Device } = file:open(Filename, [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
