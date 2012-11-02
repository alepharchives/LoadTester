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


-include("../include/records.hrl").

%% API
-export([start/2]).


%%%===================================================================
%%% API
%%%===================================================================

start(Job, Wait) ->
    inets:start(),
    lager:notice("Get workder[~p] starting, will wait: ~p",[self(), Wait]),
    timer:sleep(Wait),
    lager:notice("Get worker [~p] awake",[self()]),
    loop(Job).


loop(#job{count=Count, limit=Limit}=State)->
    case Count > (Limit-1) of
	true ->	stop(State);
	false ->
	    receive
		stop ->
		    stop(State)
	    after
		0 ->
		    work(State)
	    end
    end.

work(#job{url=Url, 
	  headers=_Headers, 
	  last_response=LastResp, 
	  register=Register,
	  delay_ms=Delay,
	  action_fn=Fn, 
	  count=Count, 
	  min=Min, 
	  max=Max, 
	  total_time=TotalTime}=State) ->
    
    {NewUrl, NewRegister} = Fn(Url, Register, LastResp),
    {TimeMs, {ok,{{"HTTP/1.1",200,"OK"},[_, _, _, _], _}}=Resp} = 
	timer:tc(fun()->httpc:request(get, {NewUrl, []},[],[]) end),
    Time = TimeMs/1000000,
    NewMin=min(Min, Time), 
    NewMax=max(Max,Time),
    NewCount = Count + 1,
    NewTotalTime = TotalTime + Time,
    timer:sleep(Delay),
    loop(State#job{url=NewUrl, 
		   last_response=Resp, 
		   register=NewRegister,
		   count=NewCount, 
		   min=NewMin, 
		   max=NewMax, 
		   total_time=NewTotalTime}).

stop(#job{min=Min, max=Max, count=Count, total_time=TotalTime})->
    Avg = TotalTime/Count,
    io:format("~nGet worker [~p] finished: min: ~p, max: ~p, avg: ~p count: ~p~n",
	      [self(), Min, Max, Avg, Count]).
    
    
