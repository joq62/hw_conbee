-define(DeviceSpecList,[
%%------------------- IKEA  ---------------------------
[
    {a,b},    
    {<<"modelid">>,<<"TRADFRIbulbE14WScandleopal470lm">>}],

    {<<"name">>,<<"blue_lamp_inglasad">>},
 {<<"state">>,[<<"bri">>,<<"colormode">>,<<"ct">>,
			<<"alert">>,<<"on">>,<<"reachable">>]},	
 {<<"type">>,<<"Color temperature light">>},
 {<<"module">>,a}

[{<<"modelid">>,<<"TRADFRI bulb E27 CWS 806lm">>},
 {<<"name">>,<<"light_color_joakim">>},
 { <<"state">>,>[<<"bri">>,
                <<"colormode">>,
                <<"ct">>,
                <<"xy">>,
                <<"effect">>,
                <<"hue">>,
	        <<"alert">>,
                <<"on">>,
                <<"reachable">>]},
  {<<"type">>,<<"Extended color light">>},
  {<<"module">>,tradfri_bulb_E27_cws_806lm}
]


-define(DeviceTypes,["lights","sensors"]).
