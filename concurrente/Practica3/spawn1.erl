%Autor:Pablo Gordillo

-module(spawn1).
-export([test/1,my_spawn/3,dormir/1]).


%% Toma el tiemstamp al empezar la ejecución,
%%  crea el proceso como normalmente y crea otro para enlazarlo con primer proceso lanzado.
%% Este segundo proceso se activa la recepción de señales como mensaje y se queda esperando a la recepción de la señal que le indique que el primer proceso ha terminado su ejecución.
%% Tras ello vuelve a obtener el timestamp para calcular el tiempo transcurrido y muestra la información por pantalla.

my_spawn(Mod,Func,Args)->
    Start = timeSpent(),
    Pid = spawn(Mod,Func,Args),
    spawn(fun()-> process_flag(trap_exit,true),
		  link(Pid),
		  receive
		      {'EXIT',Pid,R}->End = timeSpent(),
				      io:format("The process ~w has finished its execution with message ~p after ~f seconds~n",[Pid,R,End-Start])
		  end
	  end),
    Pid.


 % Función que coge el timestampo y devuelve el mismo en segundos.
timeSpent()->
    {Mega,S,Micro}=erlang:timestamp(),
    S1 = Mega*1000000,
    S2 = Micro/1000000.0,
    S+S1+S2.

%% Función de test para comprobar que se muestra adecuadamente el tiempo transcurrido.
%%Si tras ella se comprueba la información de los procesos de erlang no se encontrará ninguno asociado a spawn1.
test(Ts)->
    my_spawn(spawn1,dormir,[Ts]).

dormir(Ts)->
    timer:sleep(Ts).
