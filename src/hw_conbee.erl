%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(hw_conbee).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include("log.api").
-include("device.spec").

-define(ConbeeContainer,"deconz").
-define(SERVER,?MODULE).

%% External exports
-export([
	 %% basic
	 get/3,
	 set/3,
	


	 %% lights
	 what_lights/0,
	
	 %% sensors
	 what_sensors/0
	 
	]).


-export([
	 ping/0,
	 start_link/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------

-record(state,{device_info,
	       ip_addr,
	       ip_port,
	       crypto
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop()-> gen_server:call(?SERVER, {stop},infinity).


%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get(Name,Function,Args)->
    gen_server:call(?SERVER, {get,Name,Function,Args},infinity). 


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set(Name,Function,Args)->
    gen_server:call(?SERVER, {set,Name,Function,Args},infinity). 

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%% lights
what_lights()->
    gen_server:call(?SERVER, {what_lights},infinity).
%% sensors
what_sensors()->
    gen_server:call(?SERVER, {what_sensors},infinity).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping() ->
    gen_server:call(?SERVER, {ping}).


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
    HostName=case application:get_env(hw_conbee,test_host) of
		 undefined->
		     {ok,Host}=net:gethostname(),
		     Host;
		 {ok,TestHost}->
		     TestHost
	     end,
    {ok,[{conbee,ConbeeConfig}]}=etcd_host:get_appl_config(HostName),
    {conbee_addr,ConbeeAddr}=lists:keyfind(conbee_addr,1,ConbeeConfig),
    {conbee_port,ConbeePort}=lists:keyfind(conbee_port,1,ConbeeConfig),
    {conbee_key,ConbeeKey}=lists:keyfind(conbee_key,1,ConbeeConfig),
    application:ensure_all_started(gun),
    
    os:cmd("docker restart "++?ConbeeContainer),
    timer:sleep(5*1000),
    
    ?LOG_NOTICE("Server started ",["Servere started",node(),
				   ip_addr,ConbeeAddr,
				   ip_port,ConbeePort,
				   crypto,ConbeeKey]),
   
    {ok, #state{device_info=undefined,
	        ip_addr=ConbeeAddr,
		ip_port=ConbeePort,
		crypto=ConbeeKey}}.   
 

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

%%---------------------------------------------------------------------
%% Basic
%%---------------------------------------------------------------------
handle_call({set,Name,Function,Args},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=lib_hw_conbee:set(Name,Function,Args,ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};

handle_call({get,Name,Function,Args},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=lib_hw_conbee:get(Name,Function,Args,ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};

%%---------------------------------------------------------------------
%% Lights 
%%---------------------------------------------------------------------
handle_call({what_lights},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,what_devices,["lights",ConbeeAddr,ConbeePort,Crypto],2*5000),
    {reply, Reply, State};


%%---------------------------------------------------------------------
%%  Sensors 
%%---------------------------------------------------------------------
handle_call({what_sensors},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,what_devices,["sensors",ConbeeAddr,ConbeePort,Crypto],2*5000),
    {reply, Reply, State};

%%---------------------------------------------------------------------
%%  General 
%%---------------------------------------------------------------------


handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    ?LOG_WARNING("Unmatched signal",[Request]),
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
    ?LOG_WARNING("Unmatched signal",[Msg]),
    
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
    ?LOG_WARNING("Unmatched signal",[Info]),
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
