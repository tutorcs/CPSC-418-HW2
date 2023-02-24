https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-module(hw2_tests).

-include_lib("eunit/include/eunit.hrl").

-export([p_x/1, reduce_x/2, reduce_bu_test/1, scan_x/2, scan_bu_test/1]).
-export([cumsum/1, cumsum/2]).

p_test_() ->
  [ p_x(Args) || Args <- [{1, 6, 23}, {8, 100, 6087}, {19, 1000, 608383}]].

p_x({P, N, Expected}) ->
  { setup,
    fun() -> red:create(P) end, % set-up
    fun(W) -> red:reap(W) end, % clean up
    fun(W) -> ?_assertEqual(Expected, hw2:p(W, N)) end}.  % the test itself


% reduce_bu_test(EUnit)
%   If EUnit == true,
%     then generate a list of EUnit tests.
%   Otherwise, run the tests that EUnit would run, but print a more human
%   friendly description of the outcome (EUnit suppresses output from test
%   execution).
reduce_bu_test(EUnit) when is_boolean(EUnit) ->
  [ reduce_x(P, EUnit) || P <- [4, 8, 1, 105 ]].
reduce_bu_test_() -> reduce_bu_test(true).

reduce_x(P, EUnit) when is_integer(P) ->
  Test = { setup,
    fun() ->  red:create(P) end, % set-up
    fun(W) -> red:reap(W) end, % clean up
    fun(W) -> % run
      Data = misc:rlist(P*P + 5, 1000),
      red:update(W, data, Data),
      red:broadcast(W,
	fun(ProcState) ->
	  MyData = red:get(ProcState, data),
	  Total = hw2:reduce_bu(ProcState, lists:sum(MyData), fun(Left, Right) -> Left+Right end),
	  red:put(ProcState, total, Total)
	end),
      ExpectedTotal = lists:sum(Data),
      Expected = [ExpectedTotal || _ <- lists:seq(0, P-1)],
      Actual = red:retrieve(W, total),
      case {EUnit, Expected =:= Actual} of
        {false, true} ->
	  io:format("reduce_x(~w): passed~n  Data = ~w~n  Sum = ~w~n",
		    [P, Data, Actual]);
        {false, false} ->
	  io:format("reduce_x(~w): FAILED~n  Data = ~w~n  Sum (expected) =~w~n  Sum (from test) = ~w~n",
		    [P, Data, Expected, Actual]),
	  failed;
	{true, _} -> ?_assertEqual(Expected, Actual)
      end
    end
  },
  case EUnit of
    true -> Test;
    false -> % run the test now
      {setup, SetUp, CleanUp, Run} = Test,
      W = SetUp(),
      Result = Run(W),
      CleanUp(W),
      Result
  end.


% scan_bu_test(EUnit)
%   Like reduce_bu_test.
%   If EUnit == true, generate EUnit tests;
%   otherwise, run the tests and print the results in a human friendly way.
scan_bu_test(EUnit) when is_boolean(EUnit) ->
  [ scan_x(P, EUnit) || P <- [4, 8, 1, 105 ]].
scan_bu_test_() -> scan_bu_test(true).

scan_x(P, EUnit) when is_integer(P) ->
  Test = { setup,
    fun() ->  red:create(P) end, % set-up
    fun(W) -> red:reap(W) end, % clean up
    fun(W) -> % run
      Data = misc:rlist(P*P + 5, 1000),
      red:update(W, data, Data),
      red:broadcast(W,
	fun(ProcState) ->
	  MyData = red:get(ProcState, data),
	  AccIn = hw2:scan_bu(ProcState, 0, lists:sum(MyData), fun(Left, Right) -> Left+Right end),
	  red:put(ProcState, cumsum, cumsum(AccIn, MyData))
	end),
      AllData = lists:append(red:retrieve(W, data)),
      Expected = cumsum(AllData),
      Actual = lists:append(red:retrieve(W, cumsum)),
      case {EUnit, Expected =:= Actual} of
        {false, true} ->
	  io:format("scan_x(~w): passed~n  Data = ~w~n  CumSum = ~w~n",
		    [P, AllData, Actual]);
        {false, false} ->
	  io:format("scan_x(~w): FAILED~n  Data = ~w~n  CumSum (expected) =~w~n  Sum (from test) = ~w~n",
		    [P, AllData, Expected, Actual]),
	  failed;
	{true, _} -> ?_assertEqual(Expected, Actual)
      end
    end
  },
  case EUnit of
    true -> Test;
    false -> % run the test now
      {setup, SetUp, CleanUp, Run} = Test,
      W = SetUp(),
      Result = Run(W),
      CleanUp(W),
      Result
  end.

cumsum(AccIn, List) ->
  element(1, lists:mapfoldl(fun(X, Acc) -> Y = Acc+X, {Y, Y} end, AccIn, List)).
cumsum(List) -> cumsum(0, List).
