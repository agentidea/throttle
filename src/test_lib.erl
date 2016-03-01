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

%% API
-export([log_done/1, log_done_delay/1, log_drop/1]).

log_drop(Arg) ->
  io:format("Dropped this: ~p~n", [Arg]).

log_done(Arg) ->
  io:format("Done this: ~p~n", [Arg]).

log_done_delay(Arg) ->
  receive
    after 500 ->
      log_done(Arg)
  end.



