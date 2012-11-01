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

%% API
-export([start/1]).

-record(state, {
	  url=""  ::string(),
	  headers=[],
	  action_fn,
	  last_response="",
	  limit,
	  count=0,
	  min=999999999,
	  max=0,
	  avg=0,
	  total_time=0,
	  register
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start([Url, Headers, Limit, Fn, Wait]) ->
    inets:start(),
    lager:notice("Get workder[~p] starting, will wait: ~p",[self(), Wait]),
    timer:sleep(Wait),
    lager:notice("Get worker [~p] awake",[self()]),
    loop(#state{url=Url, headers=Headers, limit=Limit, action_fn=Fn}).


loop(#state{url=Url, 
	    headers=_Headers, 
	    last_response=LastResp, 
	    register=Register,
	    limit=Limit, 
	    action_fn=Fn, 
	    count=Count, 
	    min=Min, 
	    max=Max, 
	    total_time=TotalTime}=State) ->
    case Count =< (Limit-1) of
	true ->    
	    {NewUrl, NewRegister} = Fn(Url, Register, LastResp),
	    {TimeMs, {ok,{{"HTTP/1.1",200,"OK"},[_, _, _, _], _}}=Resp} = 
		timer:tc(fun()->httpc:request(get, {NewUrl, []},[],[]) end),
	    Time = TimeMs/1000000,
	    NewMin=min(Min, Time), 
	    NewMax=max(Max,Time),
	    NewCount = Count + 1,
	    NewTotalTime = TotalTime + Time,
	    loop(State#state{url=NewUrl, 
			     last_response=Resp, 
			     register=NewRegister,
			     count=NewCount, 
			     min=NewMin, 
			     max=NewMax, 
			     total_time=NewTotalTime});
	false ->
	    Avg = (TotalTime/Count), 
	    io:format("~nGet worker [~p] finished: min: ~p, max: ~p, avg: ~p count: ~p~n",
			 [self(), Min, Max, Avg, Count])
    end.
    
    
