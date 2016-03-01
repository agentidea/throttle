%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Mar 2016 12:12
%%%-------------------------------------------------------------------
-module(throttle_worker).
-author("bartek").

-behaviour(gen_server).

%% API
-export([start_link/2, start_job/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {fun_todo, monitor}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Monitor, Todo) ->
  gen_server:start_link(?MODULE, [Todo, Monitor], []).

start_job(Pid, Arg) ->
  gen_server:cast(Pid, {do, Arg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Todo, Monitor]) ->
  {ok, #state{fun_todo = Todo, monitor = Monitor}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This is a job to process, or actually an Arg, because we already have
%% a function to execute. Try to execute, and whatever the reason notify
%% the monitoring process that we are done with it.
%% @end
%%--------------------------------------------------------------------
handle_cast({do, Arg}, #state{fun_todo = Fun} = State) ->
  try Fun(Arg) of
    _ -> ok
  after
    throttle:confirm_job(State#state.monitor)
  end,
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
