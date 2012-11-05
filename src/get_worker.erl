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
-export([start/3]).


%%%===================================================================
%%% API
%%%===================================================================

start(Job, Wait, Tid) ->
    inets:start(),
    lager:notice("Get workder[~p] starting, will wait: ~p",[self(), Wait]),
    timer:sleep(Wait),
    lager:notice("Get worker [~p] awake",[self()]),
    loop(Job, Tid).


loop(#job{count=Count, limit=Limit}=State, Tid)->
    case Count > (Limit-1) of
	true ->	stop(State, Tid);
	false ->
	    receive
		stop ->
		    stop(State, Tid)
	    after
		0 ->
		    work(State, Tid)
	    end
    end.

work(#job{type=T,
	  name=N,
	  url=Url, 
	  headers=Headers, 
	  last_response=LastResp, 
	  register=Register,
	  delay_ms=Delay,
	  action_fn=Fn, 
	  count=Count, 
	  min=Min, 
	  max=Max, 
	  total_time=TotalTime}=State, Tid) ->
    
    {NewUrl, NewRegister} = do_action(Fn, Url, Register, LastResp),
    
    {TimeMs, 
     {ok,{{"HTTP/1.1",200,"OK"},[_, _, _, _], _}}=Resp} = 
	timer:tc(fun()->httpc:request(get, {NewUrl, Headers},[],[]) end),

    Time = TimeMs/1000000,
    NewMin=min(Min, Time), 
    NewMax=max(Max,Time),
    NewCount = Count + 1,
    NewTotalTime = TotalTime + Time,
    ets:insert(Tid, {self(), T, N, NewMin, NewMax, NewCount, NewTotalTime}),
    timer:sleep(Delay),
    loop(State#job{url=NewUrl, 
		   last_response=Resp, 
		   register=NewRegister,
		   count=NewCount, 
		   min=NewMin, 
		   max=NewMax, 
		   total_time=NewTotalTime}, Tid).

stop(#job{type=T, name=N, min=Min, max=Max, count=Count, total_time=TotalTime}, Tid)->
    Avg = case Count > 0 of true ->TotalTime/Count; false -> 0 end,
    io:format("~nGet worker [~p] finished: min: ~.3f, max: ~.3f, avg: ~.3f count: ~p~n",
	      [self(), Min, Max, Avg, Count]),
    ets:insert(Tid, {self(), T, N, Min, Max, Count, TotalTime}).
    
    
do_action(Fn, Url, Register, LastResp) ->
    case Fn of
	none ->
	    {Url, Register};
	_ when is_function(Fn) ->
	    Fn(Url, Register, LastResp)
    end.
