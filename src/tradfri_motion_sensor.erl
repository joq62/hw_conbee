%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(tradfri_motion_sensor). 
  
-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include("log.api").

 
-define(SERVER,?MODULE).
-define(Type,<<"ZHAPresence">>).

%<<"7">> =>
%      #{<<"config">> =>
%            #{<<"alert">> => <<"none">>,<<"battery">> => 74,
%              <<"delay">> => 180,<<"duration">> => 60,
%              <<"group">> => <<"7">>,<<"on">> => true,
%              <<"reachable">> => true},
%        <<"ep">> => 1,
%        <<"etag">> => <<"099be61dd3013fe3b2512528acf48402">>,
%        <<"lastannounced">> => <<"2023-06-26T05:39:20Z">>,
%        <<"lastseen">> => <<"2023-08-28T19:27Z">>,
%        <<"manufacturername">> => <<"IKEA of Sweden">>,
%        <<"modelid">> => <<"TRADFRI motion sensor">>,
%        <<"name">> => <<"protoype_ikea_motion">>,
%        <<"state">> =>
%            #{<<"dark">> => true,
%              <<"lastupdated">> => <<"2023-08-28T19:27:53.621">>,
%              <<"presence">> => true},
%        <<"swversion">> => <<"2.0.022">>,
%        <<"type">> => <<"ZHAPresence">>

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
	 is_presence/1,
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

is_presence({[],Name,WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {state_get,<<"presence">>,{[],Name,WantedNumDeviceMaps}},infinity).

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
