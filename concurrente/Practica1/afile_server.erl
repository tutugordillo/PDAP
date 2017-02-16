%%Autor: Pablo Gordillo
%%Ejercicio 1 Practica 1

-module(afile_server).
-export([start/1,loop/1]).

start(Dir)->
    register(server,spawn(afile_server,loop,[Dir])).

loop(Dir)->
    receive
	{Client,list_dir}->
	    Client!{server,file:list_dir(Dir)},
	    loop(Dir);
	{Client,{get_file,File}} ->
	    Full = filename:join(Dir,File),
	    Client !{server,file:read_file(Full)},
	    loop(Dir);
	{Client,{change_dir,AbsolutePath}} ->
	    Client !{server,"Path changed"},
	    loop(AbsolutePath)
    end.
