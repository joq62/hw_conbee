%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(hw_conbee_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(ConbeeContainer,"deconz"). 

%% External exports
-export([
	
]).


-export([
	 start/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------

-record(state,{ip_addr,
	       ip_port,
	       crypto
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%% cast

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
       
    {ok,ConbeeAddr}=application:get_env(hw_conbee_app,conbee_addr),
    {ok,ConbeePort}=application:get_env(hw_conbee_app,conbee_port),
    {ok,Crypto}=application:get_env(hw_conbee_app,conbee_key),
    
    application:ensure_all_started(gun),
    os:cmd("docker restart "++?ConbeeContainer),
    timer:sleep(5*1000),
    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Server started",
								  ip_addr,ConbeeAddr,
								  ip_port,ConbeePort,
								  crypto,Crypto]]),
    {ok, #state{ip_addr=ConbeeAddr,
		ip_port=ConbeePort,
		crypto=Crypto}}.   
 

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({set,DeviceName,DeviceState},_From, State) ->

    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,[" set request",
								  device_name,DeviceName,
								  device_state,DeviceState]]), 
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,set,[DeviceName,DeviceState,
					     ConbeeAddr,ConbeePort,Crypto],2*5000),
    {reply, Reply, State};

handle_call({get,DeviceName},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,get,[DeviceName,ConbeeAddr,ConbeePort,Crypto],2*5000),
    {reply, Reply, State};


handle_call({get_all_device_info,DeviceType},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,[node(),
								  DeviceType
								 ]]),
    Reply=rpc:call(node(),lib_hw_conbee,all_info,[DeviceType,ConbeeAddr,ConbeePort,Crypto],2*5000),
    {reply, Reply, State};

handle_call({device_info,WantedDeviceName},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,device_info,[WantedDeviceName,ConbeeAddr,ConbeePort,Crypto],2*5000),
    {reply, Reply, State};

handle_call({get_state},_From, State) ->
    Reply=State,
    {reply, Reply, State};

handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(timeout, State) -> 
    io:format("timeout ~p~n",[{?MODULE,?LINE}]), 
    
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched match~p~n",[{Info,?MODULE,?LINE}]), 
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
