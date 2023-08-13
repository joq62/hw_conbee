%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lumi_sensor_pressure).

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include("log.api").

-define(Type,<<"ZHAPressure">>).
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

num({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {num,{[],Name,WantedNumDeviceMaps}},infinity). 
 
etag({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {basic,<<"etag">>,{[],Name,WantedNumDeviceMaps}},infinity). 
lastannounced({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {basic,<<"lastannounced">>,{[],Name,WantedNumDeviceMaps}},infinity). 
lastseen({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {basic,<<"lastseen">>,{[],Name,WantedNumDeviceMaps}},infinity). 
modelid({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {basic,<<"modelid">>,{[],Name,WantedNumDeviceMaps}},infinity). 
name({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {basic,<<"name">>,{[],Name,WantedNumDeviceMaps}},infinity). 
swversion({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {basic,<<"swversion">>,{[],Name,WantedNumDeviceMaps}},infinity). 
type({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {basic,<<"type">>,{[],Name,WantedNumDeviceMaps}},infinity). 
uniqueid({[],Name,WantedNumDeviceMaps})->basic,
    gen_server:call(?SERVER, {basic,<<"uniqueid">>,{[],Name,WantedNumDeviceMaps}},infinity). 

%% State
lastupdate({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {state_get,<<"lastupdate">>,{[],Name,WantedNumDeviceMaps}},infinity).
pressure({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {state_get,<<"pressure">>,{[],Name,WantedNumDeviceMaps}},infinity). 

%% config 
is_on({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {config_get,<<"on">>,{[],Name,WantedNumDeviceMaps}},infinity). 
is_reachable({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {config_get,<<"reachable">>,{[],Name,WantedNumDeviceMaps}},infinity). 



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
handle_call({num,{[],Name,NumMaps}},_From, State) ->
    [{Num,Map}]=get_nummap(Name,NumMaps),
    Reply=binary_to_list(Num),
    {reply, Reply, State};


handle_call({basic,Key,{[],Name,NumMaps}},_From, State) ->
    [{Num,Map}]=get_nummap(Name,NumMaps),
    Reply=binary_to_list(maps:get(Key,Map)),
    {reply, Reply, State};

%% state
handle_call({state_get,Key,{[],Name,NumMaps}},_From, State) ->
    [{Num,Map}]=get_nummap(Name,NumMaps),
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(Key,DeviceMap),
    {reply, Reply, State};

%% config
handle_call({config_get,Key,{[],Name,NumMaps}},_From, State) ->
   [{Num,Map}]=get_nummap(Name,NumMaps),
    ConfigMap=maps:get(<<"config">>,Map),
    Reply=maps:get(Key,ConfigMap),
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
get_nummap(Name,NumMaps)->
    NameBin=list_to_binary(Name),
    [{Num,Map}||{Num,Map}<-NumMaps,
		?Type=:=maps:get(<<"type">>,Map),
		NameBin=:=maps:get(<<"name">>,Map)].
		   
    
