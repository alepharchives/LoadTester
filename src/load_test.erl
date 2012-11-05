%%%------------------------------------------------------------------------------
%%% @author  <Jim Rosenblum>
%%% @copyright (C) 2012, 
%%% @doc Gen_server which implements the API to load testing as well as starting
%%% the threads
%%% @end
%%% Created : 31 Oct 2012 by  <Jim Rosenblum>
%%%-----------------------------------------------------------------------------
-module(load_test).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% Module API
-export([start_link/0, 
	 set_ramp_time/1,
	 define_job/1, 
	 clear_stats/0,
	 show_jobs/0,
	 
	 start_test/0, 
	 stop_test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-include("../include/records.hrl").
-type load_spec()     :: #job{}.

-define(SERVER, ?MODULE). 

-record(state, {ramp_time_ms=30 :: pos_integer(),
		load_specs=[]   ::[load_spec()],
		pids=[],
		results,
		timer_ref}).

%%%===================================================================
%%% API
%%%===================================================================
set_ramp_time(Time) when is_integer(Time), Time >=0 ->
    gen_server:call(?MODULE, {ramp_time, Time}).

define_job(JobTuple) when is_tuple(JobTuple)->
    gen_server:call(?MODULE, {parse_config, JobTuple});
define_job(_) ->
    {error, badarg}.

clear_stats() ->
    gen_server:call(?MODULE, clear_stats).
show_jobs() ->
    gen_server:call(?MODULE, show_jobs).

start_test()->
    gen_server:call(?MODULE, start_test).

stop_test()->
        gen_server:cast(?MODULE, stop_test).
    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true), 
    Tid = ets:new(results, [public, {write_concurrency,true}]),
    
    {ok, #state{results=Tid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(start_test, _From, State) ->
    NewState = start_test(State),
    {reply, ok, NewState};

handle_call({ramp_time, Time}, _From, State) ->
    {reply, ok, State#state{ramp_time_ms=Time}};

handle_call({parse_config, PropList}, _From, State) ->
    NewState = parse_config(PropList, State),
    lager:notice("New State ~p~n",[NewState]),
    {reply, ok, NewState};

handle_call(clear_stats, _From, #state{results=Tid}=State) ->
    ets:delete_all_objects(Tid),
    {reply, ok, State};

handle_call(show_jobs, _From, #state{load_specs=Specs}=State) ->
    show_jobs(Specs),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop_test, State) ->
    stop_test(State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, #state{pids=Pids}=S) ->
    NewPids = lists:keydelete(MonitorRef, 2, Pids),
    {noreply, S#state{pids=NewPids}};

handle_info(display_stats, State) ->
    display_stats(State#state.results),
    Timer = erlang:send_after(10000, ?MODULE, display_stats),
    {noreply, State#state{timer_ref=Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_config({get, Name, N, Url, Headers, Action, Limit, Delay}, 
	     #state{load_specs=Ls}=State) ->
    ActionFn = case Action of
		   _ when is_function(Action) -> 
		       Action;
		   _Else -> 
		       none
	       end,
    NewJob = #job{type=get, name=Name, n_procs=N, url=Url, headers=Headers, 
		  action_fn=ActionFn, limit=Limit, delay_ms=Delay},

    State#state{load_specs=[NewJob|lists:keydelete(Name, #job.name, Ls)]}.

start_test(#state{load_specs=[]}=State)->
    lager:notice("No workers to spawn"),
    State;
start_test(#state{ramp_time_ms=Stagger, load_specs=Jobs, pids=Pids, results=Tid}=State)->
    lager:notice("Beginning to spawn workers"),
    F = fun(#job{n_procs=N}, Acc) -> Acc + N end,
    NJobs = lists:foldl(F, 0, Jobs),
    EachWait = erlang:round(Stagger/NJobs),
    
    NewPids = start_workers(Jobs, EachWait, EachWait, Pids, Tid),

    Timer = erlang:send_after(10000, ?MODULE, display_stats),
    State#state{pids=NewPids, timer_ref=Timer}.

start_workers([], _Wait, _WaitIncr, Pids, _Tid) ->
    lager:notice("Done spawing workers"),
    Pids;
start_workers([#job{type=get, n_procs=N}=Job|Rest], Wait, WaitIncr, Pids, Tid)->
    Limit = Wait + (N-1) * WaitIncr,
    NewPids = [spawn_monitor(get_worker, start, [Job, W, Tid]) || 
		  W <- lists:seq(Wait, Limit, WaitIncr)],
    start_workers(Rest, Limit+WaitIncr, WaitIncr, NewPids ++ Pids, Tid).


stop_test(#state{pids=Pids, results=Tid, timer_ref=Timer})->
    lager:notice("Attmepting to stop workers"),
    [(Pid ! stop) || {Pid,_} <- Pids],
    erlang:cancel_timer(Timer),
    display_stats(Tid),
    ok.

show_jobs(Specs)->
    F = fun(#job{type=T, name=N, n_procs=P, limit=L, 
		 delay_ms=D, url=U, headers=H, action_fn=A}) ->
		io:format("~nType:~p, Name:~p, N_procs:~p Loops:~p, Delay:~p~n",
			  [T, N, P, L, D]),
		io:format("Headers:~p,~nUrl:~p,~nActionFn:~p~n~n",
			  [H, U, A])
	end,
    lists:foreach(F, Specs).
    

display_stats(Tid)->
    F = fun({_Pid, get, _N, Min, Max, Count, TotalTime}, {AMin, AMax, ACount, ATT, AAvg})->
		NewMin = min(Min, AMin),
		NewMax = max(Max, AMax),
		NCount = Count + ACount,
		NTT = TotalTime + ATT,
		NewA = NTT/NCount,
		{NewMin, NewMax, NCount, NTT, NewA};
	   (_, Acc) -> Acc
	end,
    {PMin, PMax, PCount, PTotalTime, PAvg} = ets:foldl(F, {9999.99,0.0,0 ,0.0,0.0}, Tid),
    io:format("Min: ~.3f, Max: ~.3f, Avg: ~.3f, Count: ~p~n",
	      [PMin, PMax, PAvg, PCount]).
		
