%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2016 11:07
%%%-------------------------------------------------------------------
-module(throttle_test).
-author("bartek").

-define(PRT(X), ?debugFmt("~p", [X])).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    R = spawn(test_lib, count_sigs, [0, 0]),
    register(counter, R),
    {ok, TMon} = throttle:start_link(
        fun(A) -> test_lib:accept(A) end,
        fun(A) -> test_lib:reject(A) end,
        10
    ),
    ?PRT("Testing: the worker is able to process at most 20 msgs/second"),
    ?PRT("and has a max queue length of 10"),
    ?PRT("======== Sending 15 msgs slowly:"),
    ?PRT("expect: all 15 accepted"),
    test_lib:do_jobs(15, 75, TMon),
    {ok, Ac, Rej} = test_lib:get_stats(),
    ?assertEqual(Ac, 15),
    ?assertEqual(Rej, 0),
    ?PRT("======== Sending 20 msgs extremely quickly:"),
    ?PRT("expect: 10 accepted, 10 rejected"),
    test_lib:do_jobs(20, 0, TMon),
    timer:sleep(1000),
    {ok, Ac1, Rej1} = test_lib:get_stats(),
    ?assertEqual(Ac1, 25),
    ?assertEqual(Rej1, 10),
    ?PRT("======== Sending 15 msgs slowly:"),
    ?PRT("expect: all accepted"),
    test_lib:do_jobs(15, 75, TMon),
    {ok, Ac2, Rej2} = test_lib:get_stats(),
    ?assertEqual(Ac2, 40),
    ?assertEqual(Rej2, 10),
    ?assert(true).
