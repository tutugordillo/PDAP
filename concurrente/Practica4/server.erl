%%Autor:Pablo Gordillo

-module(server).
-export([start/0,loop/0,loop/3,sendAll/2,sendAllAsynch/2,players/0,stop/0,play/0,select_word/1,addWord/1,removeWord/1,showWords/0,names/0,identifiers/0]).


%%Función que comienza la ejecución del servidor.
%% Comprueba que no haya sido registrado previeamente y en ese caso, avisa al usuario.
start()->
    A = whereis(server),
    if A == undefined->
	    register(server,spawn(fun()->loop()end)),
	    io:format("Server registered~n");
       true-> io:format("The serves has been registered before~n")
    end .

%%Función que llama al bucle principal que gestionará las peticiones al servidor. Recibe 3 parametos: la lista de jugadores (vacía), una lista con palabras y la palabra actual.
loop()->
	loop([],["hola","pablo","erlang","programacion","funcional","master"],undefined),
	io:format("END~n").

%Función que ejecuta el bucle principal gestionando las peticiones al servidor.
%% Recibe la lista de jugadores conectados al mismo,la lista de palabras y la palabra actual.

loop(Players,Words,Word)->
    Self = self(),
    receive

	%%Muestra la lista de jugadores
	players->
	    io:format("The players connected to this server are : ~p~n",[Players]),
	    loop(Players,Words,Word);

	%Comienza el juego enviando a todos los jugadores de la lista la palabra a mostrar
	play->
	    case Word == undefined of
		true->
		    io:format("The game is going to start~n"),
		    WN=select_word(Words),
		    sendAll(Players,{Self,{word,WN}}),
		    loop(Players,Words,WN);
		false->
		    loop(Players,Words,Word)
	    end;

	%Finaliza la ejecución del servidor avisando a todos los jugadores de ello.
	stop->  
	    sendAllAsynch(Players,{Self,stop}),
	    io:format("Bye! The game has finished~n");

	names->
	    N = getNames(Players),
	    io:format("The names of the players connected to the server are: ~p~n",[N]),
	    loop(Players,Words,Word);

	identifiers->
	    I = getIdentifiers(Players),
	    io:format("The identifiers of the players connected to the server are: ~p~n",[I]),
	    loop(Players,Words,Word);

	%Permite añadir una palabra nueva a la lista.
	{addWord,W}->
	    loop(Players,[W|Words],Word);

	%Permite eliminar una palabra de la lista.
	{removeWord,W}->
	    NewWords = removeElement(W,Words),
	    loop(Players,NewWords,Word);

	%Muestra la lista de palabras con la que se juega.
	words->
	    io:format("Words: ~p~n",[Words]),
	    loop(Players,Words,Word);

	%Recibe una palabra enviada por el proceso From. Comprueba si es correcta o no.
	%Si es correcta avisa a todos los jugadores de quien es el ganador.
	%En caso contrario avisa al jugador From de su error.
	{From,{word,W}}->
	    case W == Word of
		true->
		    Name=lookForName(From,Players),
		    io:format("Winner: ~p. Pid ~w~n",[Name,From]),
		    sendAllAsynch(Players,{Self,{{From,Name},wins}}),
		    WN = select_word(Words),
		    sendAll(Players,{Self,{word,WN}}),
		    loop(Players,Words,WN);
		false ->
		    From!{Self,{incorrect,tryAgain}},
		    loop(Players,Words,Word)
	    end;
	
	%%Registra a un usuario. Comprueba previamente que no se encuentra registrado.
	%%En caso de estarlo, avisa al jugador.
	{From,{reg,Nombre}}->
	    Names = getNames(Players),
	    case lists:member(Nombre,Names) of
		true-> 
		    From!{Self,{reg,incorrect}},
		    loop(Players,Words,Word);
		false-> 
		    From!{Self,{reg,correct}},
		    io:format("Player ~w connected. Name:~p~n",[From,Nombre]),
		    loop([{From,Nombre}|Players],Words,Word)
	    end;
	    
	 {removePlayer,Name}->
	    Pid=lookForId(Name,Players),
	    NewPlayers=removeElement({Pid,Name},Players),
	    loop(NewPlayers,Words,Word);

	_X -> loop(Players,Words,Word)
    
    end.
		     
%%Función que dado un elemento y una lista, elimina el elemento de la mencionada lista.
removeElement(_X,[])->
    [];
removeElement(X,[X|R]) ->
    removeElement(X,R);
removeElement(X,[Y|R]) ->
    [Y|removeElement(X,R)].


%%Función que envía el mensaje Msg a todos los procesos que se encuentren en la lista Players.
sendAll(Players,Msg)->
	[P!Msg || {P,_}<-Players].

%%Crea un nuevo proceso para que el envío de mensajes sea asíncrono.
sendAllAsynch(Players,Msg)->
	spawn(fun()->sendAll(Players,Msg)end).

%%Selecciona de forma aleatoria una palabra de las existentes en la lista.
select_word(Words)->
    N = random:uniform(length(Words)),
    lists:nth(N,Words).

%Dada la lista de tuplas {Pid, Nombre} obtiene el nombre.
getNames([])->
    [];


getNames([{_,Name}|Rest])->
    [Name|getNames(Rest)].

%Dada la lista de tuplas {Pid,Nombre} obtiene los Pids    
getIdentifiers([])->
    [];

getIdentifiers([{Pid,_}|Rest])->
    [Pid|getIdentifiers(Rest)].


%%Dado un Pid y la lista de jugadores devuelve el nombre del jugador.
lookForName(_Pid,[])->
    io:format("Player is not registered");

lookForName(Pid,[{Pid,Name}|_Players])->
    Name;
lookForName(Pid,[{_P,_}|Players]) ->
    lookForName(Pid,Players).

lookForId(_Name,[])->
    io:format("Player is not registered");
lookForId(Name,[{Pid,Name}|_Players]) ->
    Pid;
lookForId(Name,[{_Pid,_Name}|Players]) ->
    lookForId(Name,Players).



%Interfaz de uso
players()->
    server!players.

stop()->
    server!stop.

play()->
    server!play.

addWord(W)->
    server!{addWord,W}.

removeWord(W)->
    server!{removeWord,W}.

showWords()->
    server!words.

names()->
    server!names.

identifiers()->
    server!identifiers.
