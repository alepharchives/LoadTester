%%%-------------------------------------------------------------------
%%% @author  <Jim Rosenblum>
%%% @copyright (C) 2012, 
%%% @doc
%%% A get worker makes a number of URL requests logging its time and
%%% statistics
%%% @end
%%% Created : 31 Oct 2012 by  <>
%%%-------------------------------------------------------------------
-module(get_worker).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  url  ::string(),
	  headers,
	  action_fn,
	  last_response,
	  limit,
	  count=0,
	  min=999999999,
	  max=0,
	  avg=0,
	  tt=0
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Args) ->
    gen_server:start_link(?MODULE, Args, []).

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
init([Url, Headers, Limit, Fn, Wait]) ->
    lager:notice("starting Get worker [~p], will wait~p",[self(), Wait]),
    inets:start(),
    {ok, #state{url=Url, 
		headers=Headers,
		limit=Limit, 
		action_fn=Fn,
		count=0
	       }, 
     Wait}.

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
handle_info(timeout, State) ->
    lager:notice("[~p] is awake",[self()]),
    work(State),
    {noreply, State};
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
work(#state{url=Url, 
	    headers=_Headers,
	    last_response=LR, 
	    limit=Limit, 
	    action_fn=Fn,
	    count=Cnt,
	    min=M, max=X, tt=T}= State) ->

    {NewUrl, LR} = Fn(Url, LR),
    {Time, Resp} = timer:tc(fun()->httpc:request(get, {NewUrl, []},[],[]) end),
    NewM=min(M, Time),
    NewX=max(X,Time),
    NC = Cnt + 1,
    NewT = T + Time,
    NAvg = (NewT/NC), 
    case Cnt < (Limit-1) of
	true ->
	    work(State#state{url=NewUrl, 
			     last_response=Resp, 
			     count=NC, 
			     min=NewM, 
			     max=NewX, 
			     avg=NAvg, tt=NewT});
	false ->
	    lager:notice("[~p] min: ~p, max: ~p, avg: ~p count: ~p",[self(), NewM, NewX, NAvg, NC])
    end.
    
    
