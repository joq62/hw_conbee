%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(hw_conbee_tests).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(hw_conbee_node ,do_test@c201).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=lights_test(),
    ok=sensors_test(),
  
    
  
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
switches_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    All=rpc:call(?hw_conbee_node,hw_conbee,get_all_device_info,["switch"],2*5000),
    io:format("All ~p~n",[{All, ?MODULE,?FUNCTION_NAME}]),

    AllNames=[Name||{Name,_Id,_DeviceType,_Map}<-All],
    io:format("AllNames ~p~n",[{AllNames, ?MODULE,?FUNCTION_NAME}]),

    Reg1=[{rpc:call(?hw_conbee_node,hw_conbee,get,[Name,"lights"],2*5000),Name}||Name<-AllNames],
    io:format("Reg1 ~p~n",[{Reg1, ?MODULE,?FUNCTION_NAME}]),

    Registred=[Name||{{ok,_},Name}<-Reg1],
    io:format("Registred ~p~n",[{Registred, ?MODULE,?FUNCTION_NAME}]),

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
sensors_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    All=rpc:call(?hw_conbee_node,hw_conbee,get_all_device_info,["sensors"],2*5000),
    io:format("All ~p~n",[{All, ?MODULE,?FUNCTION_NAME}]),

    AllNames=[Name||{Name,_Id,_DeviceType,_Map}<-All],
    io:format("AllNames ~p~n",[{AllNames, ?MODULE,?FUNCTION_NAME}]),

    Reg1=[{rpc:call(?hw_conbee_node,hw_conbee,get,[Name,"lights"],2*5000),Name}||Name<-AllNames],
    io:format("Reg1 ~p~n",[{Reg1, ?MODULE,?FUNCTION_NAME}]),

    Registred=[Name||{{ok,_},Name}<-Reg1],
    io:format("Registred ~p~n",[{Registred, ?MODULE,?FUNCTION_NAME}]),

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
lights_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    AllLights=rpc:call(?hw_conbee_node,hw_conbee,get_all_device_info,["lights"],2*5000),
    io:format("AllLights ~p~n",[{AllLights, ?MODULE,?FUNCTION_NAME}]),

    AllNames=[Name||{Name,_Id,_DeviceType,_Map}<-AllLights],
    io:format("AllNames ~p~n",[{AllNames, ?MODULE,?FUNCTION_NAME}]),

    LightsRegistred1=[{rpc:call(?hw_conbee_node,hw_conbee,get,[Name,"lights"],2*5000),Name}||Name<-AllNames],
    io:format("LightsRegistred1 ~p~n",[{LightsRegistred1, ?MODULE,?FUNCTION_NAME}]),

    LightsRegistred=[Name||{{ok,_},Name}<-LightsRegistred1],
    io:format("LightsRegistred ~p~n",[{LightsRegistred, ?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    
    pong=net_adm:ping(?hw_conbee_node),   

    ok.
