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
	 all_maps/0,
	 all_info/1,
	 call/3,


	 %% lights
	 what_lights/0,
	 info_light/1,
	 is_reachable_light/1,

	 turn_on_light/1,
	 turn_off_light/1,
	 is_on_light/1,
	 is_off_light/1,
	 set_brightness_light/2,
	 get_brightness_light/1,
	 set_color_light/2,
	 get_color_light/1,
	 %% sensors
	 what_sensors/0,
	 info_sensor/1,
	 is_reachable_sensor/1,

	 is_open_sensor/1,
	 is_dark_sensor/1,
	 is_daylight_sensor/1,
	 lightlevel_sensor/1,
	 lux_sensor/1,
	 motion_sensor/1,
	 %% Switch
	 what_switches/0,
	 info_switch/1,
	 is_reachable_switch/1,

	 is_on_switch/1, 
	 is_off_switch/1,
	 turn_on_switch/1,
	 turn_off_switch/1

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
all_maps()->
    gen_server:call(?SERVER, {all_maps},infinity). 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
all_info(DeviceType)->
    gen_server:call(?SERVER, {all_info,DeviceType},infinity). 


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
call(Name,Function,Args)->
    gen_server:call(?SERVER, {call,Name,Function,Args},infinity). 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%% lights
what_lights()->
    gen_server:call(?SERVER, {what_lights},infinity).
info_light(DeviceName)->
    gen_server:call(?SERVER, {info_light,DeviceName},infinity).
is_reachable_light(DeviceName)->
    gen_server:call(?SERVER, {is_reachable_light,DeviceName},infinity).

turn_on_light(DeviceName)->
    gen_server:call(?SERVER, {turn_on_light,DeviceName},infinity).
turn_off_light(DeviceName)->
    gen_server:call(?SERVER, {turn_off_light,DeviceName},infinity).
is_on_light(DeviceName)->
    gen_server:call(?SERVER, {is_on_light,DeviceName},infinity).
is_off_light(DeviceName)->
    gen_server:call(?SERVER, {is_off_light,DeviceName},infinity).

set_brightness_light(DeviceName,Value)->
    gen_server:call(?SERVER, {set_brightness_light,DeviceName,Value},infinity).
get_brightness_light(DeviceName)->
    gen_server:call(?SERVER, {get_brightness_light,DeviceName},infinity).
set_color_light(DeviceName,Value)->
    gen_server:call(?SERVER, {set_brightness_light,DeviceName,Value},infinity).
get_color_light(DeviceName)->
    gen_server:call(?SERVER, {get_brightness_light,DeviceName},infinity).

%% sensors
what_sensors()->
    gen_server:call(?SERVER, {what_sensors},infinity).
info_sensor(DeviceName)->
    gen_server:call(?SERVER, {info_sensor,DeviceName},infinity).
is_reachable_sensor(DeviceName)->
    gen_server:call(?SERVER, {is_reachable_sensor,DeviceName},infinity).

is_open_sensor(DeviceName)->
    gen_server:call(?SERVER, {is_open_sensor,DeviceName},infinity).
is_dark_sensor(DeviceName)->
    gen_server:call(?SERVER, {is_dark_sensor,DeviceName},infinity).
is_daylight_sensor(DeviceName)->
    gen_server:call(?SERVER, {is_daylight_sensor,DeviceName},infinity).
lightlevel_sensor(DeviceName)->
    gen_server:call(?SERVER, {lightlevel_sensor,DeviceName},infinity).
lux_sensor(DeviceName)->
    gen_server:call(?SERVER, {lux_sensor,DeviceName}).
motion_sensor(DeviceName)->
    gen_server:call(?SERVER, {motion_sensor,DeviceName}).

%% Switch
what_switches()->
    gen_server:call(?SERVER, {what_switches},infinity).
info_switch(DeviceName)->
    gen_server:call(?SERVER, {info_switch,DeviceName},infinity).
is_reachable_switch(DeviceName)->
    gen_server:call(?SERVER, {is_reachable_switch,DeviceName},infinity).

is_on_switch(DeviceName)->
    gen_server:call(?SERVER, {is_on_switch,DeviceName},infinity).
is_off_switch(DeviceName)->
    gen_server:call(?SERVER, {is_off_switch,DeviceName},infinity).
turn_on_switch(DeviceName)->
    gen_server:call(?SERVER, {turn_on_switch,DeviceName},infinity).
turn_off_switch(DeviceName)->
    gen_server:call(?SERVER, {turn_off_switch,DeviceName},infinity).


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


handle_call({all_maps},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=lib_hw_conbee:all_maps(ConbeeAddr,ConbeePort,Crypto),

    {reply, Reply, State};

handle_call({all_info,DeviceType},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
 
    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    Cmd="/api/"++Crypto++"/"++DeviceType,
    Ref=gun:get(ConnPid,Cmd),
    Reply = case gun:await_body(ConnPid, Ref) of
		{ok,Body}->
		    Maps=jsx:decode(Body,[])
	    end,
    ok=gun:close(ConnPid),
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

handle_call({is_reachable_light,DeviceName},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,is_reachable,["lights",DeviceName,ConbeeAddr,ConbeePort,Crypto],2*5000),
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

handle_call({is_reachable_sensor,DeviceName},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,is_reachable,["sensors",DeviceName,ConbeeAddr,ConbeePort,Crypto],2*5000),
    {reply, Reply, State};
%%---------------------------------------------------------------------
%%  Switches 
%%---------------------------------------------------------------------
handle_call({what_switches},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,what_devices,["switches",ConbeeAddr,ConbeePort,Crypto],2*5000),
    {reply, Reply, State};

handle_call({is_reachable_switch,DeviceName},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=rpc:call(node(),lib_hw_conbee,is_reachable,["switches",DeviceName,ConbeeAddr,ConbeePort,Crypto],2*5000),
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
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Unmatched signal",
							       Msg]]),
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
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Unmatched signal",
							       Info]]),
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
