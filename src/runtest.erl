%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Mar 2016 13:11
%%%-------------------------------------------------------------------
-module(runtest).
-author("bartek").

%% API
-export([run/0]).


run() ->
  {ok, TMon} = throttle:start_link(
    fun(A) -> test_lib:log_done_delay(A) end,
    fun(A) -> test_lib:log_drop(A) end,
    10
  ),
  io:format("~n"),
  io:format("Testing: the worker is able to process at most 2 msgs/second~n"),
  io:format("~n"),
  io:format("======== Sending 3 msgs slowly:~n"),
  sendjoba(TMon, 3).


sendjoba(TMon, 0) ->
  io:format("======== Flooding the throttle with 10 msgs/second:~n"),
  sendjob(TMon, 20);
sendjoba(TMon, I) ->
  throttle:send_job(TMon, I),
  receive
  after 2000 ->
    sendjoba(TMon, I - 1)
  end.

sendjob(TMon, 0) ->
  io:format("======== Sending another 10 msgs slowly:~n"),
  sendjobc(TMon, 10);
sendjob(TMon, I) ->
  throttle:send_job(TMon, I),
  receive
    after 100 ->
    sendjob(TMon, I - 1)
  end.

sendjobc(_, 0) ->
  ok;
sendjobc(TMon, I) ->
  throttle:send_job(TMon, I),
  receive
  after 2000 ->
    sendjobc(TMon, I - 1)
  end.
