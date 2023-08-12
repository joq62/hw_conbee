%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_hw_conbee).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("device.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([
	 what_devices/4,
	 is_reachable/5
	 
	 
	]). 


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
what_devices("lights",ConbeeAddr,ConbeePort,Crypto)->
    get_device_ids("lights",ConbeeAddr,ConbeePort,Crypto);
what_devices("sensors",ConbeeAddr,ConbeePort,Crypto)->
    get_device_ids("sensors",ConbeeAddr,ConbeePort,Crypto);
what_devices("switches",ConbeeAddr,ConbeePort,Crypto)->
    get_device_ids("switches",ConbeeAddr,ConbeePort,Crypto).

get_device_ids(DeviceType,ConbeeAddr,ConbeePort,Crypto)->
    AllInfo=all_info(DeviceType,ConbeeAddr,ConbeePort,Crypto),
    DeviceIds=[Name||{Name,NumId,ModelId,Status}<-AllInfo],
    DeviceIds.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_reachable("lights",DeviceName,ConbeeAddr,ConbeePort,Crypto)->
    check_reachable("lights",DeviceName,ConbeeAddr,ConbeePort,Crypto);
is_reachable("sensors",DeviceName,ConbeeAddr,ConbeePort,Crypto)->
    check_reachable("sensors",DeviceName,ConbeeAddr,ConbeePort,Crypto);
is_reachable("switches",DeviceName,ConbeeAddr,ConbeePort,Crypto)->
    check_reachable("switches",DeviceName,ConbeeAddr,ConbeePort,Crypto).


check_reachable(DeviceType,DeviceName,ConbeeAddr,ConbeePort,Crypto)->
    DeviceMapList=[#{device_name=>Name, device_num_id=>NumId,device_model=>ModelId,
		     device_type=>DeviceType,device_status=>Status}||{Name,NumId,ModelId,Status}<-all_info(DeviceType,ConbeeAddr,ConbeePort,Crypto),
								     DeviceName=:=Name],
    IsReachable=case DeviceMapList of
		    []->
			{error,["Eexists ",DeviceName,?MODULE,?LINE]};
		    [DeviceMap]->
			io:format("DBG DeviceMap ~p~n",[{DeviceMap,?MODULE,?LINE}]),
			 ModelId=maps:get(device_model,DeviceMap),
			[Module]=[maps:get(module,Map)||Map<-?DeviceInfo,
						  ModelId==maps:get(model_id,Map)],
			case rpc:call(node(),Module,is_reachable,[DeviceMap],5000) of
			    {badrpc,Reason}->
				{error,["unexpected error ",badrpc,Reason,DeviceType,DeviceName,?MODULE,?LINE]};
			    true->
				true;
			    false ->
				false
			end
		end,
    IsReachable.
			
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_reply(ConnPid,StreamRef)->
 %   io:format("~p~n", [{?MODULE,?LINE}]),
 %   StreamRef = gun:get(ConnPid, "/"),
    case gun:await(ConnPid, StreamRef) of
	{response, fin, Status, Headers} ->
%	    io:format(" no_data ~p~n", [{?MODULE,?LINE}]),
	    Body=[no_data];
	{response, nofin, Status, Headers} ->
%	    io:format(" ~p~n", [{?MODULE,?LINE}]),
	    {ok, Body} = gun:await_body(ConnPid, StreamRef),
	    Body
    end,
    {Status, Headers,Body}.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
all_info(DeviceType,ConbeeAddr,ConbeePort,Crypto)->
    extract_info(DeviceType,ConbeeAddr,ConbeePort,Crypto).
  
extract_info(DeviceType,ConbeeAddr,ConbeePort,Crypto)->
    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    CmdLights="/api/"++Crypto++"/"++DeviceType,
    Ref=gun:get(ConnPid,CmdLights),
    Result= get_info(gun:await_body(ConnPid, Ref)),
    ok=gun:close(ConnPid),
    Result.

get_info({ok,Body})->
    get_info(Body);
get_info(Body)->
    Map=jsx:decode(Body,[]),
    format_info(Map).

format_info(Map)->
    L=maps:to_list(Map),
 %   io:format("L=~p~n",[{?MODULE,?LINE,L}]),
    format_info(L,[]).

format_info([],Formatted)->
    Formatted;
format_info([{IdBin,Map}|T],Acc)->
    NumId=binary_to_list(IdBin),
    Name=binary_to_list(maps:get(<<"name">> ,Map)),
    ModelId=binary_to_list(maps:get(<<"modelid">>,Map)),
    State=maps:get(<<"state">>,Map),
    NewAcc=[{Name,NumId,ModelId,State}|Acc],
    format_info(T,NewAcc).

