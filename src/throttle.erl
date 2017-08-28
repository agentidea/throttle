%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, Erlang Solutions
%%% @doc
%%% A very simple throttle; when we (a) need to do a lot of synchronous jobs
%%% which may lag behind or get stuck, (b) we can't afford to wait for
%%% them to be done or timeout, and (c) we don't want to flood a process'
%%% mailbox because there is really a lot of jobs, then this is the tool.
%%% It can accept any number of jobs, which are sent to in asynchronously,
%%% but queues them only up to a given limit, then whatever comes is dropped.
%%% The function to be executed (the synchonous one) has to accept one argument.
%%% @end
%%% Created : 01. Mar 2016 12:07
%%%-------------------------------------------------------------------
-module(throttle).
-author("bartek").

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, start_link/4, send_job/2, confirm_job/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {counter, worker, fun_dropped, limit}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start the monitor - give it to functions to execute
%% the first is to be executed on every job, the other is to be executed
%% if there is an overflow and we begin dropping jobs.
%% Both args should be a one-argment fun() objects. The "drop function"
%% efaults to no-op.
%% Take care of handling errors in your functions, anything you don't catch
%% crashes the whole throttle.
%% The last argument says how many jobs may be queued until we begin to drop.
%% @end
%%--------------------------------------------------------------------
start_link(Fun_todo, Limit) ->
    start_link(Fun_todo, drop, Limit).

start_link(Fun_todo, Fun_dropped, Limit) when is_function(Fun_todo, 1)->
    gen_server:start_link(?MODULE, [Fun_todo, Fun_dropped, Limit], []);
start_link(Name, Fun_todo, Limit) when is_tuple(Name)->
    start_link(Name, Fun_todo, drop, Limit).

start_link(Name, Fun_todo, Fun_dropped, Limit) ->
    gen_server:start_link(Name, ?MODULE, [Fun_todo, Fun_dropped, Limit], []).

send_job(Pid, A) ->
    gen_server:cast(Pid, {do, A}).

confirm_job(Pid) ->
    gen_server:call(Pid, done).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Fun_todo, Fun_dropped, Limit]) ->
    {ok, Worker} = throttle_worker:start_link(self(), Fun_todo),
    {ok, #state{counter = 0, worker = Worker, fun_dropped = Fun_dropped,
        limit = Limit}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% We receive info from worker that a job was done
%% all we need to do is decrement the counter (unless something
%% was messed up and it is already zero, then we ignore it)
%% @end
%%--------------------------------------------------------------------
handle_call(done, _From, #state{counter = Ct} = State) ->
    {reply, ok, State#state{counter = max(0, Ct - 1)}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the counter - if the number of pending jobs (not confirmed by
%% the worker) is too high we just drop it by executing the "drop function"
%% (which defaults to no-op). Otherwise we send job to worker and
%% increment the counter.
%% @end
%%--------------------------------------------------------------------
handle_cast({do, Arg}, State) ->
    Ct = State#state.counter,
    NCt = if Ct < State#state.limit ->
        throttle_worker:start_job(State#state.worker, Arg),
        Ct + 1;
              true ->
                  do_apply(State#state.fun_dropped, Arg),
                  Ct
          end,
    {noreply, State#state{counter = NCt}};
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

do_apply(drop, _) ->
    ok;
do_apply(F, Arg) ->
    F(Arg).

