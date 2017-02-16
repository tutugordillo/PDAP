-module(ring).
-export([start/3,create/1,createLink/2,wait/2]).



start(M,N,Msg)->
    Pid = create(N),
    Pid!{M-1,N,Msg}.

%Crea un proceso, espera a los siguientes y se pone a esperar.

create(N)->
    spawn(fun()->
		  Proc=self(),
		  R = createLink(N-1,Proc),
		  wait(N,R) end).

%Crea todos los procesos, si es el último lo enlaza con el primero y se ponen a esperar.
    
createLink(1,Proc)->
    spawn(ring,wait,[1,Proc]);

createLink(N,Proc) ->
    spawn(fun()->
		  R = createLink(N-1,Proc),
		  wait(N,R) end).

%Gestiona el envio de mensajes. 
wait(Id,Sig)->
    receive 
	{0,0,Msg}->io:format("Proceso ~w recibe el mensaje ~p y cierra el ciclo~n",[Id,Msg]);
	
	{0,N,Msg}-> io:format("Proceso ~w pasa el mensaje ~p y muere ~n ",[Id,Msg]),
		    Sig!{0,N-1,Msg};
	
	{M,0,Msg} -> io:format("Proceso ~w pasa el mensaje ~p y comienza otra vuelta~n",[Id,Msg]),
		     Sig!{M-1,Id,Msg},
		     wait(Id,Sig);
	{M,N,Msg}->io:format("Proceso ~w pasa el mensaje ~p~n",[Id,Msg]),
		   Sig!{M,N-1,Msg},
		   wait(Id,Sig)
	end.
	
