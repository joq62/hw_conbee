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

%% --------------------------------------------------------------------
-define(DeviceTypes,["sensors","lights"]).

%% External exports
-export([
	 all_info/4,
	 device_info/4,
	 set/5,
	 get/4
	 
	]). 


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
device_info(WantedDeviceName,ConbeeAddr,ConbeePort,Crypto)->
    AllInfoSensors=all_info("sensors",ConbeeAddr,ConbeePort,Crypto),
    SensorsInfo=[#{device_name=>Name, device_num_id=>NumId,device_model=>ModelId,
		   device_type=>"sensors",device_status=>Status}||{Name,NumId,ModelId,Status}<-AllInfoSensors,
								   WantedDeviceName=:=Name],
    AllInfoLights=all_info("lights",ConbeeAddr,ConbeePort,Crypto),
    LightsInfo=[#{device_name=>Name, device_num_id=>NumId,device_model=>ModelId,
		  device_type=>"lights",device_status=>Status}||{Name,NumId,ModelId,Status}<-AllInfoLights,
								 WantedDeviceName=:=Name],
    Result=case {SensorsInfo,LightsInfo} of
               {[],[]}->
                   {error,[eexists,WantedDeviceName]};
	       {Maps,[]}->
                   {ok,Maps};
	       {[],Maps}->
		   {ok,Maps}
           end,
    Result.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
set(WantedDeviceName,DeviceState,ConbeeAddr,ConbeePort,Crypto)->
    Result=case device_info(WantedDeviceName,ConbeeAddr,ConbeePort,Crypto) of
	       {error,Reason}->
		   {error,Reason}; 
	       {ok,[Map]}->
		   DeviceType=maps:get(device_type,Map),
		   DeviceId=maps:get(device_num_id,Map),
		   Cmd="/api/"++Crypto++"/"++DeviceType++"/"++DeviceId++"/state",
		   Body=case DeviceState of
			    "on"->
				jsx:encode(#{<<"on">> => true});		   
			    "off"->
				jsx:encode(#{<<"on">> => false})
			end,
		   {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
		   StreamRef = gun:put(ConnPid, Cmd, 
				       [{<<"content-type">>, "application/json"}],Body),
		   ResultHttp=get_reply(ConnPid,StreamRef),
		   ok=gun:close(ConnPid),
		   ResultHttp
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get(WantedDeviceName,ConbeeAddr,ConbeePort,Crypto)->
    device_info(WantedDeviceName,ConbeeAddr,ConbeePort,Crypto).

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

