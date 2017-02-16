-module(server).
-export([create_server/0,init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3,finalizar/0]).
-behaviour(gen_server).

%%Función que crea el proceso servidor.
create_server()->
    {ok,PidServer}=gen_server:start({local,server},?MODULE,[],[]),
    PidServer.

%%Inicialización del proceso. El estado será una lista con los usuarios registrados.
init(ListUsers)->
    {ok,ListUsers}.

%%Recepción mensaje register. Registra al usuario añadiéndolo a la lista de usuarios y contesta al mismo con un ack.
handle_call({PidUser,register},_From,State)->
    List = register_user(PidUser,State),
    {reply,{self(),ack},List}.

%%Recepción de mensaje start, que es reenviado a todos los usuarios de la lista.
%%El estado no se ve modificado.
handle_cast({PidDownload,start,FileName},State) ->
    [gen_server:cast(U,{PidDownload,seed,FileName})||U<-State],
    {noreply,State};

%%Recepción mensaje close. Reenvía el mensaje a todos los usuarios y tras ello finaliza.
%%El estado no se ve modificado.
handle_cast(close,State) ->
    [gen_server:cast(U,close)||U<-State],
    {stop,normal,State}.

handle_info(Message,State)->
    io:format("Message received in the server is not supported.MESSAGE: ~p~n",[Message]),
    {noreply,State}.

%%Informa de la finalización del proceso.
terminate(_Reason,_State)->
    io:format("The server has been closed.~n").

code_change(_PreviousVersion,State,_Extra)->
    {ok,State}.
    

%%Función que recibe el identificador del usuario y la lista de usuarios y comprueba que dicho usuario no se encuentre registrado.    
register_user(PidUser,ListUsers)->
    case lists:member(PidUser,ListUsers) of
	true->
	    ListUsers;
	false ->
	    [PidUser|ListUsers]
    end.

%% Interfaz para finalizar el servidor
finalizar()->
    gen_server:cast(server,close).



