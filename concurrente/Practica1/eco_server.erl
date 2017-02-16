%%Autor: Pablo Gordillo
%% Ejercicio 2 Practica 1

-module(eco_server).
-export([start/0,loop/0]).

start()->
    register(ecoServer,spawn(eco,loop,[])).

loop()->
    receive
	{term,Term} ->
	    io:format("Term received: ~w~n",[Term]),
	    loop();
	stop -> 
	    io:format("Server stopped ~n")
    end.

