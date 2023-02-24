https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-module(hw2).  % template code for CPSC 418, HW2.

-export([p/2]).         % function for Question 1
-export([reduce_bu/3]). % function for Question 2
-export([scan_bu/4]).   % function for Question 2

% functions for Question 4
-export([brownie_steps/2, brownie_seq/2, brownie_max_mag/1, brownie_max_mag/2,
	 brownie_par/3, brownie_stat/3, brownie_stat/4]).

% a few utility functions
-export([gcd/2, missing_implementation/1]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                          %
%  template for Q1:  use reduce to estimate π                              %
%                                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% p(W, N) -> HowManyCoPrimeIntegersInOneToN
%   W is a worker tree created by red:create.
%   N is a non-negative integer.
%   Count the number of pairs of integers, {A, B}, 1 =< A,B =< N such A and
%   B are co-prime.  Do this in parallel using red:reduce.  In my solution,
%   each process calls red:get(ProcState, index) to find out which worker
%   it is, and uses that to determine which subset of [1,N] x [1,N] it should
%   be testing.
p(W, N) when is_pid(W), is_integer(N), 0 =< N ->
  missing_implementation([p, W, N]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                          %
%  template for Q2:  reduce (bottom up)                                    %
%                                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%reduce_bu(ProcState, LeafVal, Combine) -> Total
%  Called by the leaf processes of a worker treee created by red:create.
%    Parameters:
%      ProcState is the leaf process's 'state' (i.e. dictionary)
%      LeafVal is the leaf process's contribution to the total.
%      Combine is the (associative) function for combining values
%    Result:
%      Total is the value obtained by Combine'ing the results from all of
%        the worker processes.  Note that this also provides a barrier:
%        reduce_bu won't return for any worker in the tree until after
%        *all* workers have called reduce_bu.
%    Hints:
%      You're task it to implement reduce_bu.
%      Lucky for you, I implemented red:create to provide some bindings in
%      ProcState that are needed to solve this problem:
%        parent:  The pid of this nodes parent.  If this node is the root
%          of the worker tree, then parent is the atom 'none'.
%        children_bu:  A list of pid's for the roots of subtrees that
%          descend from this process, arranged in bottom-up order.  Consider
%          a tree with 8 processes, P0, ..., P7, and let ProcStatek denote the
%          ProcState for Pk.  Then,
%            red:get(ProcState0, children_bu) -> [P1, P2, P4].
%            red:get(ProcState2, children_bu) -> [P3].
%            red:get(ProcState4, children_bu) -> [P5, P6].
%            red:get(ProcState6, children_bu) -> [P7].
%            red:get(ProcStatek, children_bu) -> [], for k in [1,3,5,7].
%          You could get this by trying (at the Erlang prompt):
%            1> W8 = red:create(8).  % create a worker tree with 8 workers
%            <0.81.0>
%            2> red:retrieve(W8, fun(PS) ->
%                   {{index, red:get(PS, index)},
%                    {pid, self()},
%                    {children_bu, red:get(PS, children_bu)}}
%                 end, false).
%            [[[{{index,0},{pid,<0.81.0>},{children_bu,[<0.84.0>,<0.83.0>,<0.82.0>]}},
%               {{index,1},{pid,<0.84.0>},{children_bu,[]}}],
%              [{{index,2},{pid,<0.83.0>},{children_bu,[<0.87.0>]}},
%               {{index,3},{pid,<0.87.0>},{children_bu,[]}}]],
%             [[{{index,4},{pid,<0.82.0>},{children_bu,[<0.86.0>,<0.85.0>]}},
%               {{index,5},{pid,<0.86.0>},{children_bu,[]}}],
%              [{{index,6},{pid,<0.85.0>},{children_bu,[<0.88.0>]}},
%               {{index,7},{pid,<0.88.0>},{children_bu,[]}}]]]
%          Note that the children of process 0 (i.e. index=0, pid=<0.81.0>) are
%          processes 1 (pid=84), 2 (pid=83), and 4 (pid=82).  You can work out
%          the others.
%      red:reduce that you used in Q1 implements the top-down version of
%      reduce.  If you read and understand that code, that may help you
%      figure out how to implement reduce_bu.
%
%      Your solution should be *much* shorter than this comment.

reduce_bu(ProcState, LeafVal, Combine) ->
  % Hint: my solution just calls a helper:
  %     reduce_bu(Val, Combine, Children_bu, ParentPid).
  %   Of course, you then need to implement reduce_bu/4.
  % Try hw2_tests:reduce_x(4, false) to run a simple test.
  missing_implementation([reduce_bu, ProcState, LeafVal, Combine]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                          %
%  template for Q3:  scan (bottom up)                                      %
%                                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% scan_bu(ProcState, AccIn, LeafVal, Combine) 
scan_bu(ProcState, AccIn, LeafVal, Combine) ->
  % Hint: very similar to reduce_bu.
  % Try hw2_tests:scan_x(4, false) to run a simple test.
  missing_implementation([scan_bu, ProcState, AccIn, LeafVal, Combine]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                          %
%  template for Q4:  random walk                                           %
%                                                                          %
%  First, I provide a sequential implementation:                           %
%    brownie_steps(N, Alpha) -> a list of N steps with the distribution    %
%                                  with parameter Alpha                    %
%    brownie_seq(Steps, Pos0) -> a list of N positions when taking the     %
%                                  the steps in list Steps, starting       %
%                                  from position Pos0.                     %
%    brownie_max_mag(Seq) -> the furthest point from the origin in Seq.    %
%    brownie_stats(N, Alpha, M) -> compute brownie_max_mag for M sequences %
%                                    where the steps are from              %
%                                    brownie_steps(N, Alpha).  Return      %
%                                      [{mean, Mean}, {std, Std}]          %
%                                    the mean and standard deviation.      %
%                                                                          %
%  Then, the template:                                                     %
%    brownie_par(W, N, Alpha) -> parallel version of brownnie_max_mag.     %
%    Of course, you can use the functions from the sequential version in   %
%    your solution.                                                        %
%                                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

brownie_steps(N, Alpha) ->
  Pi = 4.0*math:atan(1.0),
  Theta = misc:rlist(N, 2*Pi),
  X = [     case X of
	      0.0 -> -100.0;
	      _ -> math:log(X)
	    end / Alpha
	||  X <- misc:rlist(N, 1.0) ],
  [ {R*math:cos(Th), R*math:sin(Th)} || {R, Th} <- lists:zip(X, Theta) ].

brownie_seq(Steps, Pos0) when is_list(Steps), is_tuple(Pos0) ->
  lists:mapfoldl(
    fun({DX, DY}, {X,Y}) -> 
	Pos = {X+DX, Y+DY},
	{Pos, Pos}
    end,
    Pos0, Steps);
brownie_seq(N, Alpha) when is_integer(N), is_float(Alpha) ->
  {Seq, _} = brownie_seq(brownie_steps(N, Alpha), {0,0}),
  Seq.

brownie_max_mag(Seq) ->
  lists:foldl(
    fun({X,Y}, Max) ->
	max(Max, math:sqrt(X*X + Y*Y))
    end, 0, Seq).
brownie_max_mag(N, Alpha) -> brownie_max_mag(brownie_seq(N, Alpha)).

brownie_stat(N, Alpha, M) ->
  Acc = stat:accum([brownie_max_mag(N, Alpha) || _ <- lists:seq(1,M)]),
  [{mean, stat:mean(Acc)}, {std, stat:std(Acc)}].

brownie_stat(W, N, Alpha, M) ->
  Acc = stat:accum([brownie_par(W, N, Alpha) || _ <- lists:seq(1,M)]),
  [{mean, stat:mean(Acc)}, {std, stat:std(Acc)}].

brownie_par(W, N, Alpha) ->
  % see the problem statement if you'd like some hints.
  missing_implementation([brownie_par, W, N, Alpha]).


% A few utilities to make sure the code compiles without warnings.
% If you try running one of the functions that is required for the 
% assignment but haven't replaced the template code with an actual
% solution, we'll print an error.
% I provided gcd/2 for Q1 -- it's a cut-and-paste from hw1.erl.
gcd(A, 0) when is_integer(A) -> abs(A);
gcd(0, B) when is_integer(B) -> abs(B);
gcd(A, B) when is_integer(A), A < 0 -> gcd(-A, B);
gcd(A, B) when is_integer(B), B < 0 -> gcd(A, -B);
gcd(A, B) when is_integer(A), is_integer(B), A >= B -> gcd_tail(A, B);
gcd(A, B) when is_integer(A), is_integer(B) -> gcd_tail(B, A).

gcd_tail(A, 0) -> A;
gcd_tail(A, B) -> gcd_tail(B, A rem B).

missing_implementation([FunAtom | Args]) ->
  io:format("missing or incomplete implementations for ~w(", [FunAtom]),
  case Args of
    [Arg1 | ArgTl] ->
      io:format("~p", [Arg1]),
      [ io:format(", ~p", [Arg]) || Arg <- ArgTl ];
    [] -> ok;
    _ -> io:format("garbled_args(~w)", [Args])
  end,
  io:format(")~n"),
  error([missing_implementation, FunAtom, Args]).
