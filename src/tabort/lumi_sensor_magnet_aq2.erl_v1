%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lumi_sensor_motion_aq2).
  
-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include("log.api").

 
-define(SERVER,?MODULE).


% <<"10">> =>
%      #{<<"config">> =>
%            #{<<"battery">> => 100,<<"duration">> => 90,<<"on">> => true,
 %             <<"reachable">> => true,<<"temperature">> => 2900},
%        <<"ep">> => 1,
%        <<"etag">> => <<"10f0c92d5f3a40eb0a8fc3c7c545ea71">>,
%        <<"lastannounced">> => <<"2022-12-20T20:22:40Z">>,
%        <<"lastseen">> => <<"2023-08-20T14:01Z">>,
%        <<"manufacturername">> => <<"LUMI">>,
%        <<"modelid">> => <<"lumi.sensor_motion.aq2">>,
%        <<"name">> => <<"protype_motion_aqara">>,
%        <<"state">> =>
%            #{<<"lastupdated">> => <<"2023-08-20T14:01:32.639">>,
%              <<"presence">> => false},
%        <<"swversion">> => <<"20170627">>,
%        <<"type">> => <<"ZHAPresence">>,
%        <<"uniqueid">> => <<"00:15:8d:00:01:dd:a2:b8-01-0406">>},


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
is_presence({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {is_presence,{[],WantedNumDeviceMaps}},infinity). 

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

handle_call({is_presence,{[],[{_Num,Map}]}},_From, State) ->
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(<<"is_presence">>,DeviceMap),
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
