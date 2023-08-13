%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(tradfri_control_outlet).
 
-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include("log.api").

 
-define(SERVER,?MODULE).


% <<"3">> =>
%      #{<<"etag">> => <<"49f06dd06302beebb3ff57e0ee354bb9">>,
%        <<"hascolor">> => false,
%        <<"lastannounced">> => <<"2023-04-26T05:52:42Z">>,
%        <<"lastseen">> => <<"2023-08-20T14:51Z">>,
%        <<"manufacturername">> => <<"IKEA of Sweden">>,
%        <<"modelid">> => <<"TRADFRI control outlet">>,
%        <<"name">> => <<"switch_lamp_balcony">>,
%        <<"state">> =>
%            #{<<"alert">> => <<"none">>,
%              <<"on">> => false,
%               <<"reachable">> => true},
%        <<"swversion">> => <<"2.0.024">>,
%        <<"type">> => <<"On/Off plug-in unit">>,
%        <<"uniqueid">> => <<"cc:86:ec:ff:fe:7e:f3:27-01">>},


%% External exports
-export([
	 %% basic
	
	 num/1,
	 etag/1,
	 hascolor/1,
	 lastannounced/1,
	 lastseen/1,
	 modelid/1,
	 name/1,
	 swversion/1,
	 type/1,
	 uniqueid/1,

	 %% state
	 is_alert/1,
	 is_on/1,
	 turn_on/1,
	 turn_off/1,
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
hascolor({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {hascolor,{[],WantedNumDeviceMaps}},infinity).
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
is_alert({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {is_alert,{[],WantedNumDeviceMaps}},infinity). 
is_on({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {is_on,{[],WantedNumDeviceMaps}},infinity). 
turn_on({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {turn_on,{[],WantedNumDeviceMaps}},infinity). 
turn_off({[],WantedNumDeviceMaps})->
    gen_server:call(?SERVER, {turn_off,{[],WantedNumDeviceMaps}},infinity). 
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

handle_call({hascolor,{[],[{_Num,Map}]}},_From, State) ->
    Reply=binary_to_list(maps:get(<<"hascolor">>,Map)),
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

handle_call({is_alert,{[],[{_Num,Map}]}},_From, State) ->
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(<<"alert">>,DeviceMap),
    {reply, Reply, State};

handle_call({is_on,{[],[{_Num,Map}]}},_From, State) ->
    DeviceMap=maps:get(<<"state">>,Map),
    Reply=maps:get(<<"on">>,DeviceMap),
    {reply, Reply, State};

%%-- set

handle_call({turn_on,{[],[{Num,Map}]}},_From, State) ->
    Reply={binary_to_list(Num),<<"on">>,true},
    {reply, Reply, State};

handle_call({turn_off,{[],[{Num,Map}]}},_From, State) ->
    Reply={binary_to_list(Num),<<"on">>,false},
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
