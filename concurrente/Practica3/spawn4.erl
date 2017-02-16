%Autor: Pablo Gordillo

-module(spawn4).
-export([start/1,alive/1,monitoriza/2,stop/0]).

%%Función que espera la recepción del mensaje stop para terminar. Si no lo recibe imprime cada Ts segundos el mensaje correspondiente.
alive(Ts)->
    receive
	stop->void
    after Ts->
	    io:format("I'm alive!~n"),
	    alive(Ts)
    end.

%%Función principal que lanza el proceso que ejecuta alive y otro proceso que lo monitorice.
start(Ts)->
    Pid = spawn(spawn4,alive,[Ts]),
    register(ialive,Pid),
    spawn(fun()->
		  monitoriza(Pid,Ts)end),
    Pid.


%%Función que monitoriza al proceso Pid. En caso de que la ejecución de este finalice debidamente imprime por pantalla un mensaje indicándolo y finaliza.
% En caso de que el proceso correspondiente se caiga, lo imprime por pantalla y lo relanza volviendolo a registrar.
monitoriza(Pid,Ts)->
    MonRef = monitor(process,Pid),
    receive
	{'DOWN',MonRef,process,Pid,normal}->
	    io:format("Process ~w says bye bye ~n",[Pid]);
	 {'DOWN',MonRef,process,Pid,Reason}->
	    io:format("Process ~w crashes. Reason: ~p~n",[Pid,Reason]),
	    NewPid = spawn(fun()->
				   alive(Ts)end),
	    register(ialive,NewPid),
	    io:format("Reloaded function in process ~w~n",[NewPid]),
	    monitoriza(NewPid,Ts)
    end.

%%Interfaz utilizada para terminar la ejecución.
stop()->
    ialive!stop.
