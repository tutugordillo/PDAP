%%Autor: Pablo Gordillo
%%Ejercicio 1 Practica 1

-module(afile_client).
-export([ls/0,get_file/1,cd/1]).

ls()->
    server!{self(),list_dir},
    receive
	{server,FileList}->
	    FileList
    end.

get_file(File)->
    
    server!{self(),{get_file,File}},
    receive
	{server,Content}->
	    Content
    end.

cd(AbsolutePath)->
    server!{self(),{change_dir,AbsolutePath}},
    receive
	{server,Msg}->
	    Msg
    end.
