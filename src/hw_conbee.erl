%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(hw_conbee).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(SERVER,list_to_atom(atom_to_list(?MODULE)++"_server")).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------


-export([
	 %lights
	 set/2,
	 get/1,
	 device_info/1,
	 get_all_device_info/1
	 % 

	]).


-export([
	
	 
	 get_state/0,
	 ping/0

	]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
device_info(WantedDeviceName)->
    gen_server:call(?SERVER, {device_info,WantedDeviceName}).


set(DeviceName,DeviceState)->
    gen_server:call(?SERVER, {set,DeviceName,DeviceState}).


get(DeviceName)->
    gen_server:call(?SERVER, {get,DeviceName}).

get_all_device_info(DeviceType)->
    gen_server:call(?SERVER, {get_all_device_info,DeviceType}).




ping() ->
    gen_server:call(?SERVER, {ping}).

get_state() ->
    gen_server:call(?SERVER, {get_state}).

%% ====================================================================!
%% External functions
%% ====================================================================!


%% ====================================================================
%% Internal functions
%% ====================================================================
