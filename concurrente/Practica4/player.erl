%Autor: Pablo Gordillo

-module(player).
-export([start/2,loop/1,write_word_send/2]).


%%Función con la que comienza el jugador. Recibe el nodo en que se encuentra el server.
%% Manda un mensaje al servidor habisando de su conexión y espera una respuesta.
%% Añade un timeout y en caso de no recibir respuesta pasado ese tiempo avisa al usuario.

start(Node,Nombre)->
    Self = self(),
    {server,Node}!{Self,{reg,Nombre}},
    receive
	{From,{reg,correct}}->
	    io:format("Connected!~n"),
	    loop(From);
	{_From,{reg,incorrect}} ->
	    io:format("Impossible to connect. The player is already registered~n")
    after 2500->
	    io:format("[TIME OUT] Impossible to connect due to an unknown reason.~n")
    end.


%Función principal que se encarga de la gestión de los mensajes del cliente:
%% *stop :finaliza el proceso.
%% *{Player, wins}: Informa de que el jugador Player ha ganado. Modifica el mensaje en función de si el usuario ha ganado o no.
%% *{word,W}: Recibe la palabra que tiene que escribir.
%% *{incorrect,tryAgain}: La palabra enviada al servidor no es la correcta. Avisa al usuario y comienza un nuevo intento.

loop(From)->
    Self = self(),
    receive
	{From,stop}->
	    io:format("The game has finished~n");
	{From,{{Player,Name},wins}} ->
	    case Player == Self of
		true->
		    io:format("Congrats!You win!~n");
		false->
		    io:format("The player ~p ~w wins!~n",[Name,Player])
	    end,
	    loop(From);
	{From,{word,W}} ->
	    io:format("Write word: ~p~n",[W]),
	    write_word_send(From,Self),
	    loop(From);
	{From,{incorrect,tryAgain}} ->
	    io:format("Try again...:~n"),
	    write_word_send(From,Self),
	    loop(From)
    end.
	    

%%Captura la palabra introducida por el usuario y se la envía al proceso From.
write_word_send(From,Self)->
    spawn(fun() ->
		  W0 = io:get_line("> "),
		  W1 = string:substr(W0,1,string:len(W0)-1),
		  From!{Self,{word,W1}}
					
	  end).

    
