-module(server).
-export([create_server/0,start_server/0,finalizar/0]).

%%Función que crea el proceso servidor.
create_server()->
    Pid =spawn(?MODULE,start_server,[]),
    register(server,Pid),
    Pid.

%%Función que llama al bucle de recepción de mensajes.
start_server()->
    loop_server([]).

%%Bucle de tratamiento de mensajes recibidos.
%%Recibe como parámetro una lista con los usuarios registrados.
loop_server(ListUsers)->
    receive
	{PidUser,register}->
	    List=register_user(PidUser,ListUsers),
	    PidUser!{self(),ack},
	    loop_server(List);
	{PidDownload,start,FileName} ->
	    [U!{PidDownload,seed,FileName}|| U<-ListUsers],
	    loop_server(ListUsers);
	close -> [U!close|| U<-ListUsers],
		 io:format("The server has been closed.~n")
    end.

%%Función que recibe el identificador del usuario y la lista de usuarios y comprueba que dicho usuario no se encuentre registrado.
register_user(PidUser,ListUsers)->
    case lists:member(PidUser,ListUsers) of
	true->
	    ListUsers;
	false ->
	    [PidUser|ListUsers]
    end.

finalizar()->
    server!close.
