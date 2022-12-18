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


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=test_1(),
    
				
     
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    AllSensors=rd:rpc_call(hw_conbee,hw_conbee,get_all_device_info,["sensors"],2000),
    io:format("AllSensors ~p~n",[{AllSensors,?MODULE,?FUNCTION_NAME}]),
    AllLights=rd:rpc_call(hw_conbee,hw_conbee,get_all_device_info,["lights"],2000),
    io:format("AllLights ~p~n",[{AllLights,?MODULE,?FUNCTION_NAME}]),
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
-define(ClusterSpec,"prototype_c201").
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=application:set_env([{oam,[{cluster_spec,?ClusterSpec}]}]),
    
    
    {ok,_}=db_etcd_server:start(),
    db_etcd:install(),
    ok=db_appl_instance:create_table(),
    ok=db_cluster_instance:create_table(),
    
    {ok,ClusterDir}=db_cluster_spec:read(dir,?ClusterSpec),
    case filelib:is_dir(ClusterDir) of
	true->
	    ok;
	false->
	    os:cmd("rm -rf "++ClusterDir),
	    ok=file:make_dir(ClusterDir)
    end,
    {ok,_}=nodelog_server:start(),
    {ok,_}=resource_discovery_server:start(),
    {ok,_}=connect_server:start(),
    {ok,_}=appl_server:start(),
    {ok,_}=pod_server:start(),
    ok=application:start(oam),

    ok=oam:new_db_info(),
    NodesToConnect=db_cluster_instance:nodes(connect,?ClusterSpec),
    [pong|_]=[net_adm:ping(Node)||Node<-NodesToConnect],
    
    LocalTypeList=[oam,db_etcd,nodelog],
    [rd:add_local_resource(LocalType,node())||LocalType<-LocalTypeList],
                   %% Make it available for oam - debug
    TargetTypeList=[hw_conbee],
    [rd:add_target_resource_type(Type)||Type<-TargetTypeList],
    rd:trade_resources(),
    
    timer:sleep(3000),
    ok.
