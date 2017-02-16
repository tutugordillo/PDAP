%%Autor: Pablo Gordillo

-module(superv).
-export([init/1,start/1]).

-behaviour(supervisor).

start({Dir,N})->
    supervisor:start_link({local,sup},?MODULE,{Dir,N}).


%%Función que genera un supervisor de tres procesos trabajadores, uno por cada servidor visto anteriormente en clase.
init({Dir,N})->
    {ok,{{one_for_one,2,1},
	 [{directorio,{afile_server,start,[Dir]},permanent,5000,worker,[afile_server]},
	  {area,{area_server,start,[]},permanent,5000,worker,[area_server]},
	  {ticket,{ticket_server,start,[N]},permanent,5000,worker,[ticket_server]} 
	 ]
	}
    }.

