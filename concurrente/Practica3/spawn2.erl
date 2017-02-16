%Autor:Pablo Gordillo

-module(spawn2).
-export([my_spawn/3,on_exit/2,test1/1,test2/1,sleep/1]).

%%Se lanza el proceso con spawn y se llama a on_exit para crear el monitor y con una función anónima como argumento.
%%Esta función recibe la razón de finalización, calcula el nuevo timestamp e imprime la información por pantalla.
%%
my_spawn(Mod,Func,Args)->
    Start = timeSpent(),
    Pid = spawn(Mod,Func,Args),
    on_exit(Pid,fun(Reason)-> End = timeSpent(),
			      io:format("The process ~w has finished its execution with message ~p in ~f seconds~n",[Pid,Reason,End-Start])
		end),
    Pid .

%Función on_exit para monitorizar un proceso.
on_exit(Pid,Fun)->
    spawn(fun()->
		  Ref = monitor(process,Pid),
		  receive
		      {'DOWN',Ref,process,Pid,Why}->
			  Fun(Why)
		  end
	  end).

timeSpent()->
    {Mega,S,Micro}=erlang:timestamp(),
    S1 = Mega*1000000,
    S2 = Micro/1000000.0,
    S+S1+S2.

%%Función para comprobar que funciona. Duerme el proceso Ts segundos y posteriormente imprimira por pantalla el Pid, junto a Ts segundos y como mensaje de finalización normal.
test1(Ts)->
    my_spawn(spawn2,sleep,[Ts]).

%%Función para comprobar que funciona. Duerme el proceso Ts segundos y le envía la señal de kill.
%%Esto provoca que el proceso finalice su ejecución sacando por pantalla el tiempo y como mensaje killed
test2(Ts)->
    Pid = my_spawn(spawn2,sleep,[Ts]),
    timer:sleep(1000),
    exit(Pid,kill).

sleep(Ts)->
    timer:sleep(Ts).
