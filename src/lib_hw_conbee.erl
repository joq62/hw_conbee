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
%-include("device.spec").
%% --------------------------------------------------------------------

%% External exports
-export([

%	 all_light_maps/3,
%	 all_sensor_maps/3,
%	 all_maps/3,
	 get/6,
	 get_maps/4,
	 set_state/7
	]). 


%% ====================================================================
%% External functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get(Name,Function,Args,ConbeeAddr,ConbeePort,Crypto)->
     NameBin=list_to_binary(Name),
    Result=case sd:call(etcd,etcd_zigbee_device,member,[NameBin],5000) of
	       false->
		   {error,["Not exists ",Name,?MODULE,?LINE]};
	       true ->
		   {ok,DeviceType}=sd:call(etcd,etcd_zigbee_device,get_device_type,[NameBin],5000),
		   {ok,Module}=sd:call(etcd,etcd_zigbee_device,get_module,[NameBin],5000),
		   Maps=get_maps(DeviceType,ConbeeAddr,ConbeePort,Crypto),
		   Keys=maps:keys(Maps),
		   NumDeviceMaps=[{Num,maps:get(Num,Maps)}||Num<-Keys]
		   %rpc:call(node(),Module,Function,[{Args,Maps}],2*5000)
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%all_maps(Ip,Port,Crypto)->
%    MapsLists=[get_maps(DeviceType,Ip,Port,Crypto)||DeviceType<-?DeviceTypes],
%    Nums=maps:keys(MapsLists),
%    NumDeviceMaps=[{Num,maps:get(Num,MapsLists)}||Num<-Nums],
%    NumDeviceMaps.


get_maps(DeviceType,Ip,Port,Crypto)->
    {ok, ConnPid} = gun:open(Ip,Port),
    Cmd="/api/"++Crypto++"/"++DeviceType,
    Ref=gun:get(ConnPid,Cmd),
    MapsList = case gun:await_body(ConnPid, Ref) of
		{ok,Body}->
		    jsx:decode(Body,[])
	    end,
    ok=gun:close(ConnPid),
    MapsList.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_state(Id,Key,Value,DeviceType,Ip,Port,Crypto)->
    Cmd="/api/"++Crypto++"/"++DeviceType++"/"++Id++"/state",
    Body=jsx:encode(#{Key => Value}),
    {ok, ConnPid} = gun:open(Ip,Port),
    StreamRef = gun:put(ConnPid, Cmd, 
			[{<<"content-type">>, "application/json"}],Body),
    Result=lib_conbee:get_reply(ConnPid,StreamRef),
    ok=gun:close(ConnPid),
    Result.
