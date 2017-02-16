-module(ring2).
-export([start/3,loop/2]).

% comienza el anillo.
% el mensaje Msg da M vueltas por N procesos
% crea primero los N procesos y uno de ellos recibe los datos
start(M,N,Msg) ->
	Pid = create(N),
	Pid ! {M-1,N,Msg}.

% crea el primer proceso y espera a recibir el siguiente para empezar el loop
create(N) ->
	  spawn(fun() -> Self = self(),loop(N,create(N-1,Self)) end).
% crea los procesos restantes y le pasan al ultimo el primero para que sepa a quien va
create(1,Primero)->
	  spawn(fun() -> loop(1,Primero) end);
create(N,Primero)->
	  spawn(fun() -> loop(N,create(N-1,Primero)) end).

% es igual para todos los procesos, la casuistica hace la magia de las vueltas
loop(Id,Next) ->
	io:format("Process ~p is waiting ~p ~n",[Id,Next]), 
	 receive
		{0,0,Msg} -> 
			io:format("Process ~p: Receive ~p~n Y SE PARA EL ENVIO~n",[Id,Msg]);
		{0,N,Msg} -> 
			io:format("Process ~p: Receive ~p~n Y MUERE ~n",[Id,Msg]),
			Next ! {0, N-1, Msg};
		{M,0,Msg} -> 
			io:format("Process ~p: Receive ~p~n Y OTRA VUELTA~n",[Id,Msg]),
			Next ! {M-1, Id, Msg},
			loop(Id,Next);
		{M,N,Msg} -> 
			io:format("Process ~p: Receive ~p~n",[Id,Msg]),
			Next ! {M, N-1, Msg},
			loop(Id,Next)
	end.

