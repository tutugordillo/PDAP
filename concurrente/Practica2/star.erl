-module(star).
-export([start/3,esperar/2,esperar_centro/3]).


%%Crea el centro, el cual se encargará de crear el resto de procesos.
start(M,N,Msg)->
    spawn(fun()-> Nodos = crear(N-1,[],self()),
		  esperar_centro(M,Msg,Nodos) end).

%%Crea el resto de procesos añadiendo sus identificadores en una lista.
crear(0,L,_)->
    L;
crear(N,L,Centro) ->
    Pid = spawn(star,esperar,[N,Centro]),
    crear(N-1,[Pid|L],Centro).

%%Funcionalidad del centro, envía los mensajes y espera a su recepción para el envío de la siguiente tanda.
esperar_centro(1,Msg,Nodos)->
    enviar(self(),Msg,1,Nodos),
    esperarMsg(Nodos);
esperar_centro(M,Msg,Nodos) ->
    enviar(self(),Msg,M,Nodos),
    [esperarMsg(P) || P<-Nodos],
    esperar_centro(M-1,Msg,Nodos).

enviar(Rec,Msg,N,Nodos)->
    [Y!{Rec,{Msg,N}} || Y<-Nodos].

esperarMsg(P)->
    receive
	{P,_}->io:format("Contestacion del proceso ~p~n",[P])
    end.

%%Funcionalidad de los nodos. Esperan la recepcion del mensaje y contestan confirmando su recepcion.
	    
esperar(Id,Centro)->    
    receive
	{Centro,{Msg,1}} ->
	    io:format("Proceso ~p recibe mensaje ~w por ultima vez~n",[Id,Msg]),
	    Centro!{self(),ok};
	{Centro,{Msg,_N}}->
	    io:format("Proceso ~p recibe mensaje ~w~n",[Id,Msg]),
	    Centro!{self(),ok},
	    esperar(Id,Centro)
    end.
