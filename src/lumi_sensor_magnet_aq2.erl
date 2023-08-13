%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lumi_sensor_magnet_aq2).
   
-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include("log.api").

 
-define(SERVER,?MODULE).
-define(Type,<<"ZHAOpenClose">>).

% <<"8">> =>
%      #{<<"config">> =>
%            #{<<"battery">> => 100,<<"on">> => true,
%              <<"reachable">> => true,<<"temperature">> => 2300},
%        <<"ep">> => 1,
%        <<"etag">> => <<"a284af1c61fe85377a7ef9d3d3152899">>,
%        <<"lastannounced">> => null,
%        <<"lastseen">> => <<"2023-08-27T18:11Z">>,
%        <<"manufacturername">> => <<"LUMI">>,
%        <<"modelid">> => <<"lumi.sensor_magnet.aq2">>,
%        <<"name">> => <<"prototype_door_sensor">>,
%        <<"state">> =>
%            #{<<"lastupdated">> => <<"2023-08-27T18:11:25.960">>,
%              <<"open">> => false},
%        <<"swversion">> => <<"20161128">>,
%        <<"type">> => <<"ZHAOpenClose">>,
%        <<"uniqueid">> => <<"00:15:8d:00:06:89:6d:bb-01-0006">>}


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
	 is_open/1,
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

num({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {num,{[],Name,NumDeviceMaps}},infinity). 
 
etag({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {etag,{[],Name,NumDeviceMaps}},infinity). 
lastannounced({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {lastannounced,{[],Name,NumDeviceMaps}},infinity). 
lastseen({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {lastseen,{[],Name,NumDeviceMaps}},infinity). 
modelid({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {modelid,{[],Name,NumDeviceMaps}},infinity). 
name({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {name,{[],Name,NumDeviceMaps}},infinity). 
swversion({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {swversion,{[],Name,NumDeviceMaps}},infinity). 
type({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {type,{[],Name,NumDeviceMaps}},infinity). 
uniqueid({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {uniqueid,{[],Name,NumDeviceMaps}},infinity). 

%% State
lastupdate({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {lastupdate,{[],Name,NumDeviceMaps}},infinity).
is_open({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {is_presence,{[],Name,NumDeviceMaps}},infinity). 

%% config 
is_on({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {is_on,{[],Name,NumDeviceMaps}},infinity). 
is_reachable({[],Name,NumDeviceMaps})->
    gen_server:call(?SERVER, {is_reachable,{[],Name,NumDeviceMaps}},infinity). 



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
    [{Num,_Map}]=lib_hw_conbee:get_nummap(Name,?Type,NumMaps),
    Reply=binary_to_list(Num),
    {reply, Reply, State};

handle_call({basic,Key,{[],Name,NumMaps}},_From, State) ->
    [{_Num,Map}]=lib_hw_conbee:get_nummap(Name,?Type,NumMaps),
    Reply=binary_to_list(maps:get(Key,Map)),
    {reply, Reply, State};

%% state_get

handle_call({state_get,Key,{[],Name,NumMaps}},_From, State) ->
    [{_Num,Map}]=lib_hw_conbee:get_nummap(Name,?Type,NumMaps),
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(Key,DeviceMap),
    {reply, Reply, State};

%% config
handle_call({config_get,Key,{[],Name,NumMaps}},_From, State) ->
    [{_Num,Map}]=lib_hw_conbee:get_nummap(Name,?Type,NumMaps),
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
