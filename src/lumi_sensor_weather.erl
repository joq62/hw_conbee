%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lumi_sensor_weather).
  
-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include("log.api").

 
-define(SERVER,?MODULE).

% <<"4">> =>
 %     #{<<"config">> =>
  %          #{<<"battery">> => 82,<<"offset">> => 0,<<"on">> => true,
  %            <<"reachable">> => false},
 %       <<"ep">> => 1,
 %       <<"etag">> => <<"027c6b414ce7ec0b5d61b21b79046e3c">>,
 %       <<"lastannounced">> => null,
 %       <<"lastseen">> => <<"2023-06-22T00:38Z">>,
 %       <<"manufacturername">> => <<"LUMI">>,
 %       <<"modelid">> => <<"lumi.weather">>,
 %       <<"name">> => <<"sensor_temp_inglasade">>,
 %       <<"state">> =>
 %           #{<<"humidity">> => 4523,
 %             <<"lastupdated">> => <<"2023-04-29T06:39:56.034">>},
%        <<"swversion">> => <<"3000-0001">>,
%        <<"type">> => <<"ZHAHumidity">>,
%        <<"uniqueid">> => <<"00:15:8d:00:06:e3:cf:fe-01-0405">>},
 % <<"5">> =>
 %     #{<<"config">> =>
  %          #{<<"battery">> => 82,<<"offset">> => 0,<<"on">> => true,
   %           <<"reachable">> => false},
  %      <<"ep">> => 1,
  %      <<"etag">> => <<"a562b136f587fe6a41185f66de0da6cb">>,
  %      <<"lastannounced">> => null,
  %      <<"lastseen">> => <<"2023-06-22T00:38Z">>,
  %      <<"manufacturername">> => <<"LUMI">>,
  %      <<"modelid">> => <<"lumi.weather">>,
  %      <<"name">> => <<"sensor_temp_inglasade">>,
%        <<"state">> =>
 %           #{<<"lastupdated">> => <<"2023-04-29T06:50:14.161">>,
 %             <<"temperature">> => 1088},
 %       <<"swversion">> => <<"3000-0001">>,
 %       <<"type">> => <<"ZHATemperature">>,
 %       <<"uniqueid">> => <<"00:15:8d:00:06:e3:cf:fe-01-0402">>},

%  <<"6">> =>
%      #{<<"config">> =>
%            #{<<"battery">> => 82,<<"offset">> => 0,<<"on">> => true,
%              <<"reachable">> => false},
%        <<"ep">> => 1,
%        <<"etag">> => <<"a562b136f587fe6a41185f66de0da6cb">>,
%        <<"lastannounced">> => null,
%        <<"lastseen">> => <<"2023-06-22T00:38Z">>,
%        <<"manufacturername">> => <<"LUMI">>,
%        <<"modelid">> => <<"lumi.weather">>,
%        <<"name">> => <<"sensor_temp_inglasade">>,
%        <<"state">> =>
%            #{<<"lastupdated">> => <<"2023-04-29T07:49:43.887">>,
%              <<"pressure">> => 1024},
%        <<"swversion">> => <<"3000-0001">>,
%        <<"type">> => <<"ZHAPressure">>,
%        <<"uniqueid">> => <<"00:15:8d:00:06:e3:cf:fe-01-0403">>},


%% External exports
-export([
	 %% basic
	
	 num/1,
	 etag/1,
	 lastannounced/1,
	 lastseen/1,
	 modelid/1,
	 name/1,
	 swversion/1,
	 type/1,
	 uniqueid/1,

	 %% state
	 lastupdate/1,
	 pressure/1,
	 humidity/1,
	 temperature/1,
	 
	 %% config
	 is_on/1,
	 is_reachable/1,
	 
	 all_info/1
	 

	]).


-export([
	 ping/0,
	 start_link/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------

-record(state,{
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
all_info(RawMap)->
    gen_server:call(?SERVER, {all_info,RawMap},infinity). 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

num({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {num,{[],WantedNumDeviceMaps}},infinity). 
 
etag({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {etag,{[],WantedNumDeviceMaps}},infinity). 
lastannounced({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {lastannounced,{[],WantedNumDeviceMaps}},infinity). 
lastseen({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {lastseen,{[],WantedNumDeviceMaps}},infinity). 
modelid({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {modelid,{[],WantedNumDeviceMaps}},infinity). 
name({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {name,{[],WantedNumDeviceMaps}},infinity). 
swversion({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {swversion,{[],WantedNumDeviceMaps}},infinity). 
type({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {type,{[],WantedNumDeviceMaps}},infinity). 
uniqueid({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {uniqueid,{[],WantedNumDeviceMaps}},infinity). 

%% State
lastupdate({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {lastupdate,{[],WantedNumDeviceMaps}},infinity).
humidity({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {humidity,{[],WantedNumDeviceMaps}},infinity). 
pressure({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {pressure,{[],WantedNumDeviceMaps}},infinity). 
temperature({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {temperature,{[],WantedNumDeviceMaps}},infinity). 

%% config 
is_on({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {is_on,{[],WantedNumDeviceMaps}},infinity). 
is_reachable({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {is_reachable,{[],WantedNumDeviceMaps}},infinity). 



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
   
    
    ?LOG_NOTICE("Server started ",["Servere started",node()]),
   
    {ok, #state{}}.   
 

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
handle_call({num,{[],[{Num,Map}]}},_From, State) ->
    Reply=binary_to_list(Num),
    {reply, Reply, State};

handle_call({modelid,{[],[{_Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"modelid">>,Map)),
    {reply, Reply, State};

handle_call({etag,{[],[{_Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"etag">>,Map)),
    {reply, Reply, State};

handle_call({lastannounced,{[],[{_Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"lastannounced">>,Map)),
    {reply, Reply, State};

handle_call({lastseen,{[],[{Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"lastseen">>,Map)),
    {reply, Reply, State};

handle_call({name,{[],[{_Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"name">>,Map)),
    {reply, Reply, State};

handle_call({swversion,{[],[{_Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"swversion">>,Map)),
    {reply, Reply, State};

handle_call({type,{[],[{_Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"type">>,Map)),
    {reply, Reply, State};

handle_call({uniqueid,{[],[{_Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"uniqueid">>,Map)),
    {reply, Reply, State};

%% state
handle_call({lastupdate,{[],[{_Num,Map}]}},_From, State) ->
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(<<"lastupdate">>,DeviceMap),
    {reply, Reply, State};

handle_call({humidity,{[],[{_Num,Map}]}},_From, State) ->
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(<<"humidity">>,DeviceMap),
    {reply, Reply, State};


handle_call({pressure,{[],[{_Num,Map}]}},_From, State) ->
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(<<"pressure">>,DeviceMap),
    {reply, Reply, State};

handle_call({temperature,{[],[{_Num,Map}]}},_From, State) ->
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(<<"temperature">>,DeviceMap),
    {reply, Reply, State};

%% config
handle_call({is_on,{[],[{_Num,Map}]}},_From, State) ->
    ConfigMap=maps:get(<<"config">>,Map),
    Reply=maps:get(<<"on">>,ConfigMap),
    {reply, Reply, State};

handle_call({is_reachable,{[],[{_Num,Map}]}},_From, State) ->
    ConfigMap=maps:get(<<"config">>,Map),
    Reply=maps:get(<<"reachable">>,ConfigMap),
    {reply, Reply, State};

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