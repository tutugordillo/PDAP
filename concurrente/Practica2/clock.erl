-module(clock).
-export([start/2,stop/0]).



%%Comprobamos si el proceso está refistrado. En tal caso informamos al usuario
%% y no lanzamos ningún otro proceso. En caso de que no exista proceso registrado
%% con ese nombre lo creamos.

start(Time,Fun)->
    L = whereis(clock),
    if L == undefined ->
	    register(clock,spawn(fun()->
					 tick(Time,Fun)end));
       true -> io:format("You have a process registered with this identifier~n")
    end.


stop()->
    clock!stop.

tick(Time,Fun)->
    receive
	stop->
	    void
    after Time->
	    Fun(),
	    tick(Time,Fun)
    end.
