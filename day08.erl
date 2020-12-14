-module(day08).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Code = lists:map(fun parse/1, Input),
  { err, Ans } = exec(Code),
  io:format("~B~n", [Ans]).

run2(Input) ->
  Code = lists:map(fun parse/1, Input),
  { ok, Ans } = exec_or_rewrite(Code),
  io:format("~B~n", [Ans]).

exec_or_rewrite(Code) -> exec_or_rewrite(Code, 0, 0, sets:new()).
exec_or_rewrite(Code, Acc, Cur, _)
  when length(Code) =:= Cur -> { ok, Acc };
exec_or_rewrite(Code, Acc, Cur, Done) ->
  Done_ = sets:add_element(Cur, Done),
    case lists:nth(Cur + 1, Code) of
      { nop, X } -> case exec(Code, Acc, Cur + X, Done_) of
          { ok, Acc_ } -> { ok, Acc_ };
          _ -> exec_or_rewrite(Code, Acc, Cur + 1, Done_)
        end;
      { acc, X } -> exec_or_rewrite(Code, Acc + X, Cur + 1, Done_);
      { jmp, X } -> case exec(Code, Acc, Cur + 1, Done_) of
          { ok, Acc_ } -> { ok, Acc_ };
          _ -> exec_or_rewrite(Code, Acc, Cur + X, Done_)
        end
  end.

exec(Code) -> exec(Code, 0, 0, sets:new()).
exec(Code, Acc, Cur, _)
  when length(Code) =:= Cur -> { ok, Acc };
exec(Code, Acc, Cur, Done) ->
  case sets:is_element(Cur, Done) of
    false ->
      Done_ = sets:add_element(Cur, Done),
      case lists:nth(Cur + 1, Code) of
        { nop, _ } -> exec(Code, Acc, Cur + 1, Done_);
        { acc, X } -> exec(Code, Acc + X, Cur + 1, Done_);
        { jmp, X } -> exec(Code, Acc, Cur + X, Done_)
      end;
    true -> { err, Acc }
  end.
  
parse(Line) ->
  case string:split(Line, " ") of
    [ "nop", V ] -> { nop, list_to_integer(V) };
    [ "acc", V ] -> { acc, list_to_integer(V) };
    [ "jmp", V ] -> { jmp, list_to_integer(V) } 
  end.


input() ->
  { ok, Device } = file:open("day08-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
