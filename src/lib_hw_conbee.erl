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


%% External exports
-export([
	 all_info/4,
	 set/6,
	 get/5
	 
	]). 


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
set(DeviceId,DeviceType,DeviceState,ConbeeAddr,ConbeePort,Crypto)->
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
    Result=get_reply(ConnPid,StreamRef),
    ok=gun:close(ConnPid),
    Result.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get(DeviceId,DeviceType,ConbeeAddr,ConbeePort,Crypto)->
    DeviceInfoList=[{Name,NumId,ModelId,DeviceState}||{Name,NumId,ModelId,DeviceState}<-all_info(ConbeeAddr,
												 ConbeePort,
												 Crypto,
												 DeviceType),DeviceId=:=Name],
    Result=case DeviceInfoList  of
	       []->
		   {error,[eexists,DeviceType,DeviceId]};
	       List->
		   {ok,List}
	   end,
    Result.

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
all_info(ConbeeAddr,ConbeePort,Crypto,DeviceType)->
    extract_info(ConbeeAddr,ConbeePort,Crypto,DeviceType).
  
extract_info(ConbeeAddr,ConbeePort,Crypto,DeviceType)->
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

