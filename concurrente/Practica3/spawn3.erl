%Autor:Pablo Gordillo

-module(spawn3).
-export([my_spawn/4,test1/1,test2/1,sleep/1]).

%%Función que lanza un proceso con spawn, y un segundo proceso que lo monitoriza. Este segundo proceso se queda esperando la terminación
%%del primero en el receive y cuando se produce imprime el mensaje por pantalla.
%% En caso de que no se produzca antes de Time envía la señal de kill al proceso monitorizado y espera la recepción de la señal de 'DOWN'
%%imprimiendo también por pantalla el mensaje.
my_spawn(Mod,Func,Args,Time)->
    Pid = spawn(Mod,Func,Args),
    spawn(fun()->Ref = monitor(process,Pid),
		 receive
		     {'DOWN',Ref,process,Pid,Reason}->
			 io:format("Process ~w has finished its execution before ~w seconds. The reason of its termination is ~p ~n",[Pid,Time,Reason])
		 after Time->
			 exit(Pid,kill),
			 receive
			     {'DOWN',Ref,process,Pid,Reason}->
				 io:format("The process ~w has been killed after ~w seconds. Reason:~p~n",[Pid,Time,Reason])
			 end
		 end
	  end),
    Pid.

%%Comprobación del funcionamiento. En este caso se ejecuta con normalidad ya que Time del after(2*Ts) es el doble que lo que tarda en ejecutarse la función sleep(Ts).
test1(Ts)->
    my_spawn(spawn3,sleep,[Ts],2*Ts).

%%Comprobación del funcionamiento. En este caso se envía la señal de kill ya que Time del after(Ts) es la mitad de lo que tarda en ejecutarse la función sleep(2*Ts).
test2(Ts)->
    my_spawn(spawn3,sleep,[2*Ts],Ts).

sleep(Ts)->
    timer:sleep(Ts).
