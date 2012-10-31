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
-export([start_link/0, set_state/1, start_test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-record(load_spec, {
	  type=get     :: get|put,
	  url          :: string(),
	  http_headers :: [tuple()],
	  action_fn    :: function(),
	  n_procs      :: pos_integer(),
	  limit        :: pos_integer()
	 }).
-type load_spec() :: #load_spec{}.


-record(state, {ramp_time_ms=30 :: pos_integer(),
		load_specs=[]   ::[load_spec()]}).

%%%===================================================================
%%% API
%%%===================================================================
set_state(Proplist) when is_list(Proplist)->
    gen_server:call(?MODULE, {parse_config,Proplist});
set_state(_) ->
    {error, badarg}.


start_test()->
    gen_server:call(?MODULE, start_test).
    
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
    {ok, #state{}}.

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
    start_test(State),
    {reply, ok, State};
handle_call({parse_config, PropList}, _From, State) ->
    NewState = parse_config(PropList, State),
    lager:notice("New State ~p~n",[NewState]),
    {reply, ok, NewState};

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


parse_config([], State) ->
    State;
parse_config([{ramp_time_ms, T} |Rest], State) when is_integer(T), T > 0 ->
    parse_config(Rest, State#state{ramp_time_ms=T});
parse_config([{get, Url, Headers, Action, N, Limit}|Rest],#state{load_specs=Ls}=State) ->
    ActionFn = case Action of
		   _ when is_function(Action) -> 
		       Action;
		   _Else -> 
		       fun(AUrl,AResp) -> {AUrl, AResp} end
	       end,
    SpecList = [#load_spec{type=get,
			   url=Url,
			   http_headers=Headers,
			   action_fn=ActionFn,
			   n_procs = N,
			  limit=Limit} | Ls],
    parse_config(Rest,State#state{load_specs=SpecList});
parse_config([_|Rst], State) ->
    parse_config(Rst, State).


start_test(#state{ramp_time_ms=Stagger, load_specs=Jobs})->
    F = fun(#load_spec{n_procs=N}, Acc) -> Acc + N end,
    NJobs = lists:foldl(F, 0, Jobs),
    EachWait = erlang:round(Stagger/NJobs),
    
    start_workers(Jobs, EachWait, EachWait).

start_workers([], _Wait, _WaitIncr) ->
    lager:notice("Done spawing workers");
start_workers([#load_spec{type=get, n_procs=N}=Job|Rest], Wait, WaitIncr)->
    Limit = Wait + (N-1) * WaitIncr,
    [spawn(get_worker, start, [[Job#load_spec.url, 
			       Job#load_spec.http_headers,
			       Job#load_spec.limit,
			       Job#load_spec.action_fn,
			       W]]) || W <- lists:seq(Wait, Limit, WaitIncr)],
    start_workers(Rest, Limit+WaitIncr, WaitIncr).


    


