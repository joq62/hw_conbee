%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(zigbee_handler).


-define(DeviceTypes,["lights","sensors"]). 

%% API
-export([call/7]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
call(Name,Type,Function,Args,Ip,Port,Crypto)->
    NameBin=list_to_binary(Name),
    MapsLists=[get_maps(DeviceType,Ip,Port,Crypto)||DeviceType<-?DeviceTypes],
    Nums=maps:keys(MapsLists),
    NumDeviceMaps=[{Num,maps:get(Num,MapsLists)}||Num<-Nums],
    SpecificMap=[{Num,DeviceMap}||{Num,DeviceMap}<-NumDeviceMaps,
				  NameBin=:=maps:get(<<"name">>,DeviceMap),
				  Type=:=maps:get(<<"type">>,DeviceMap)],
    Result=case SpecificMap of
	       []->
		   {error,["Not existing ",Name,?MODULE,?LINE]};
	       [{Num,DeviceMap}] ->
		   do_call,
		   {Num,DeviceMap};
	       Error ->
		   {error,["Unmatched signal ",Error,Name,?MODULE,?LINE]}
	   end,			   
    Result.


get_maps(DeviceType,Ip,Port,Crypto)->
    {ok, ConnPid} = gun:open(Ip,Port),
    Cmd="/api/"++Crypto++"/"++DeviceType,
    Ref=gun:get(ConnPid,Cmd),
    MapsList = case gun:await_body(ConnPid, Ref) of
		{ok,Body}->
		    Maps=jsx:decode(Body,[])
	    end,
    ok=gun:close(ConnPid),
    MapsList.




%%%===================================================================
%%% Internal functions
%%%===================================================================
