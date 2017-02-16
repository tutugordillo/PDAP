%%Autor: Pablo Gordillo

-module(gensup).
-export([start_tree/1,init/1]).

-behaviour(supervisor).

% Introduce aquí la función start_tree, que reciba un número N indicando
% la profundidad del árbol, y devuelva una tupla {ok, Pid}, donde Pid
% es el identificador del proceso del nivel superior.

start_tree(N)->
    supervisor:start_link(?MODULE,N).


%%En caso de ser uno, el supervisor lanzará los dos procesos trabajadores.
init(1)->
    process_flag(trap_exit,true),
    {ok,
     {{one_for_one,2,1},
      [
       {ticketIz,{ticket_server,start,[7]},permanent,5000,worker,[ticket_server]},
       {ticketDer,{ticket_server,start,[4]},permanent,5000,worker,[ticket_server]}
      ]
     }
    };

%%Si la N no es uno el supervisor generará otros dos supervisores, que llamarán a al función start con el valor N-1.
init(N)->
    process_flag(trap_exit,true),
    {ok,
     {{one_for_one,2*N,1},
      [{hijoIz,{gensup,start_tree,[N-1]},permanent,5000,supervisor,[gensup]},
       {hijoDer,{gensup,start_tree,[N-1]},permanent,5000,supervisor,[gensup]}
      ]
     }
    }.
