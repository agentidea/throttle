%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Mar 2016 13:07
%%%-------------------------------------------------------------------
-module(test_lib).
-author("bartek").
-include_lib("eunit/include/eunit.hrl").

-define(PRT(X), ?debugFmt("~p", [X])).

%% API
-export([do_jobs/3, accept/1, reject/1, count_sigs/2, get_stats/0]).

accept(_) ->
    receive
        after 50 ->
            whereis(counter) ! accepted
    end.

reject(_) ->
    whereis(counter) ! rejected.


get_stats() ->
    whereis(counter) ! {get, self()},
    receive
        [A, R] ->
            {ok, A, R};
        Else ->
            {error, Else}
    after 1000 ->
        {error, timeout}
    end.


count_sigs(A, R) ->
    receive
        accepted ->
            count_sigs(A + 1, R);
        rejected ->
            count_sigs(A, R + 1);
        {get, Pid} ->
            Pid ! [A, R],
            count_sigs(A, R)
    end.

do_jobs(0, _, _) ->
    ok;
do_jobs(I, 0, Pid) ->
    throttle:send_job(Pid, whatever),
    do_jobs(I - 1, 0, Pid);
do_jobs(I, Interval, Pid) ->
    throttle:send_job(Pid, whatever),
    receive
        after Interval ->
        do_jobs(I - 1, Interval, Pid)
    end.