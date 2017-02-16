%%Autor: Pablo Gordillo
%%Ejercicio 3 Practica 1


-module(clock).
-export([reloj/1,stop/0,writeTime/1]).

reloj(Time)->
    register(clock,spawn(clock,writeTime,[Time])).

writeTime(Time)->
    receive
	stop->
	    void
    after Time->
	    {_,Hour} = calendar:local_time(),
	    io:format("Hora: ~w~n",[Hour]),
	    writeTime(Time)
    end.
		
stop()-> clock!stop. 
    
