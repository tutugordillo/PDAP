%%Autor: Pablo Gordillo

-module(job_server).
-export([testFuncion/0,prueba/1,iniciar/0,enviar_trabajo/1,obtener_trabajo/0,trabajo_terminado/1,detener_servidor/0,init/1, handle_call/3,
         handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).
-define(SERVERNAME, ?MODULE).


%%Funciones utilizadas para comprobar el funcionamiento del ejercicio.

testFuncion() ->
    iniciar(),
    obtener_trabajo(),
    enviar_trabajo(fun()->prueba(1)end),
    obtener_trabajo(),
    enviar_trabajo(fun()->prueba(3)end),
    enviar_trabajo(fun()->prueba(4)end),
    [obtener_trabajo() || _ <-lists:seq(1,7)].


prueba(N) ->
    io:format("Ejecutando tarea ~p~n",[N]),
    receive
    after 1000 -> case random:uniform(4) of
		      N ->
			  io:format("Finalizando tarea ~p~n",[N]);
		      _ ->
			  prueba(N)
		  end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% User Interface %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Función que inicializa el servidor.
%%El estado estará formado por dos listas,
%%la primera la lista de trabajos pendientes y la segunda de trabajos ejecutados.
iniciar()->
    io:format("Server started~n"),
    gen_server:start_link({local,?SERVERNAME},?MODULE,[],[]).

%%Función que envía la petición para añadir un nuevo trabajo al servidor.
%%No se espera respuesta->cast.
enviar_trabajo(F)->
    gen_server:cast(?SERVERNAME,{nuevo_trabajo,F}).


%%Función con spawn para evitar que la shell se quede colgada.

%% obtener_trabajo()->
%%     spawn(fun()->case gen_server:call(?SERVERNAME,obtener_trabajo) of
%% 		     no ->
%% 			 io:format("There is not a job for you.\n");
%% 		     {Ref,F} ->
%% 			 F(),
%% 			 trabajo_terminado(Ref)
%% 		 end
%% 	  end).

%%Función que permite al usuario pedir la ejecución de un nuevo trabajo. En caso de que exista lo ejecuta y una vez finaliza, avisa mediante traajo_terminado.
obtener_trabajo()->
    case gen_server:call(?SERVERNAME,obtener_trabajo) of
	no ->
	    io:format("There is not a job for you.\n");
	{Ref,F} ->
	    F(),
	    trabajo_terminado(Ref)
    end.

%%Envía la petición al servidor avisando de que el trabajo asociado a Ref a finalizado su ejecución.
trabajo_terminado(Ref)->
   case gen_server:call(?SERVERNAME,{trabajo_terminado,Ref}) of
       ok-> 
	   io:format("The job with reference ~p has finished.\n",[Ref]);
       error ->
	   io:format("The job with reference ~p has not finished due to an error.\n",[Ref])
   end.

%%Interfaz que permite al usuario finalizar la ejecución del servidor.
detener_servidor()->
    gen_server:cast(?SERVERNAME,parar_server).
    
%%%%%%%%%%%%%%%%%%%%%%%
%%Gen Server Callbacks%
%%%%%%%%%%%%%%%%%%%%%%%

%Independientemente del argumento que recibe, el servidor se inicializa. Su estado será una tupla de dos listas,
%la primera contendrá los trabajos enviados al servidor y la segunda los trabajos que se estan ejecutando.

init(_)->
    {ok,{[],[]}}.

%%Realiza el tratamiento de la adición de un nuevo trabajo, añadiéndolo a la lista de trabajos pendientes.    
handle_cast({nuevo_trabajo,F},{P,E})->
    {noreply,{P++[F],E}};

%%Envía la señal de stop para finalizar la ejecución y pasa al callback terminate.
handle_cast(parar_server,{P,E})->
    {stop,normal,{P,E}};

%%En caso de recibirse una petición no contemplada se avisa al usuario por pantalla.
handle_cast(_,State)->
    io:format("Wrong request.\n"),
    {noreply,State}.

%%Tratamiento de la petición de obtener un trabajo.
%% Si la lista de trabajos pendientes P está vacia, se contesta con el mensaje no indicando la ausencia de trabajos.
%% En caso contrario se genera una referencia, se le asigna al primer trabajo de la lista de pendientes y se elimina el trabajo de esta lista.
%% Se añade una tupla {Ref,Trabajo} a la lista de tareas en ejecución E, devolviendo el nuevo estado.
handle_call(obtener_trabajo,{From,_Ref},{P,E})->
    case P of
	[]-> {reply,no,{P,E}};
	[F|List] ->  Ref = make_ref(),
		     io:format("Job assigned to ~p with reference ~w.\n",[From,Ref]),
		     {reply,{Ref,F},{List,[{Ref,From}|E]}}
    end;

%%Gestiona la petición para comprobar si un trabajo ha finalizado.
%%En caso de que se encuentre en la lista E, responde y lo elimina de la misma.
%%Si no, avisa al usuario y el estado no se ve modificado.
handle_call({trabajo_terminado,Ref},{From,_R},{P,E})->
    case lists:keyfind(Ref,1,E) of
	{Ref,From}-> {reply,ok,{P,lists:keydelete(Ref,1,E)}};
	false -> {reply,error,{P,E}}
    end;

%%En caso de recibirse una petición no contemplada se avisa al usuario por pantalla.
handle_call(_,_From,State)->
    io:format("Request is not handled.\n"),
    {reply,error,State}.
	    
%%Callback handle_info.			 
handle_info(_Message, State)->
    io:format("Handle Info Callback.\n"),
    io:format("Unexpected message.\n"),
    {noreply,State}.

%%Callback terminate. Es llamado cuando se utiliza la función detener_servidor().
terminate(Reason,State)->
    io:format("Terminate Callback.\n"),
    io:format("The server finishes its execution. Reason: ~p .\n",[Reason]),
    io:format("State: ~p.\n",[State]),
    ok.

%%Callback code_change.
code_change(_PreviousVersion, State, _Extra) -> 
    io:format("Code Change Callback.\n"),
    {ok, State}.
