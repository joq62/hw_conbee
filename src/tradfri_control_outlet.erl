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

%% Basic
num(RawMap)->
    gen_server:call(?SERVER, {num,RawMap},infinity). 
etag(RawMap)->
    gen_server:call(?SERVER, {etag,RawMap},infinity). 
hascolor(RawMap)->
    gen_server:call(?SERVER, {hascolor,RawMap},infinity).
lastannounced(RawMap)->
    gen_server:call(?SERVER, {lastannounced,RawMap},infinity). 
lastseen(RawMap)->
    gen_server:call(?SERVER, {lastseen,RawMap},infinity). 
modelid(RawMap)->
    gen_server:call(?SERVER, {modelid,RawMap},infinity). 
name(RawMap)->
    gen_server:call(?SERVER, {name,RawMap},infinity). 
swversion(RawMap)->
    gen_server:call(?SERVER, {swversion,RawMap},infinity). 
type(RawMap)->
    gen_server:call(?SERVER, {type,RawMap},infinity). 
uniqueid(RawMap)->
    gen_server:call(?SERVER, {uniqueid,RawMap},infinity). 

%% State
is_alert(RawMap)->
    gen_server:call(?SERVER, {is_alert,RawMap},infinity). 
is_on(RawMap)->
    gen_server:call(?SERVER, {is_on,RawMap},infinity). 
turn_on(RawMap)->
    gen_server:call(?SERVER, {turn_on,RawMap},infinity). 
turn_off(RawMap)->
    gen_server:call(?SERVER, {turn_off,RawMap},infinity). 
is_reachable(RawMap)->
    gen_server:call(?SERVER, {is_reachable,RawMap},infinity). 


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
