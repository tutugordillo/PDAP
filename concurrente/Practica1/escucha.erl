%%Autor: Pablo Gordillo
%%Ejercicio 4 Practica 1

-module(escucha).
-export([start/0,listen/0,send/1,stop/0]).

start()->
    register(escuchaPr,spawn(escucha,listen,[])).

listen()->
    receive
	{Sender,Msg} ->
	    io:format("Message: ~w~n",[Msg]), 
	    Sender!{escuchaPr,ok},
	    listen();
	stop ->
	    io:format("Process finished~n")
    after 5000->
	    io:format("I am still waiting for a message~n"),
	    listen()
    end.

send(Msg)->
    escuchaPr!{self(),Msg},
    receive 
	{escuchaPr,ok}->ok
    end.

stop()->
    escuchaPr!stop.
