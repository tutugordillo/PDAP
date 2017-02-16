%Autor:Pablo Gordillo

-module(spawn6).
-export([my_spawn_all_kill/3,spawn_all/3,monitorizar/1,control/4,test1/1,test2/1,test3/1,test4/0,test/0,delete/5,deleteList/2]).

%% Función principal que recibe una lista de Modulos, Funciones y Argumentos respectivamente.
%% Esta función comprueba que la longitud de las tres listas sea la misma,es decir,que todos los argumentos para cada una de las funciones que se va a lanzar estén definidos. Notese que la lista Argumentos será una lista de listas, ya que contendrá los posibles argumentos para cada una de las funciones (ver llamada a test al final).

%% Esta función lanza un proceso por cada una de las funciones con el método usual y tras ello lanza un nuevo proceso que se encargará de monitorizarlos a todos.
%% Este nuevo proceso tiene una llamada a monitorizar, que recibe la lista de Pids y crea un monitor para cada proceso y llama a control con la lista de Pids y las listas anteriores.

my_spawn_all_kill(Mods,Funcs,Args)->
    case (length(Mods)==length(Funcs)) and (length(Funcs)== length(Args)) of
	true->
	    Pids= spawn_all(Mods,Funcs,Args),
	    spawn(fun()->monitorizar(Pids),control(Pids,Mods,Funcs,Args) end),
	    Pids;
	false -> io:format("Incorrect number of arguments~n")
    end.


%%Función que recorre las listas y lanza los procesos para cada una de las funciones
spawn_all([],[],[])->
    [];
spawn_all([M|Mods],[F|Funcs],[A|Args]) ->
    Pid = spawn(M,F,A),
    [Pid|spawn_all(Mods,Funcs,Args)].


%%Función que recibe una lista de Pids y crea un monitor para cada uno de esos procesos.
monitorizar(Pids)->
    [erlang:monitor(process,P)||P<-Pids].

%%Función que monitoriza los procesos. En caso de que el proceso finalice de forma normal, lo indica, lo borra de la lista y  sigue controlando al resto.
%% En caso de que finalice de forma anormal, lo indica, mata al resto de procesos y espera a la confirmación de los mismos para evitar un bucle infinito (ya que si no estos mensajes se quedarán en el buzón del monitor y en el momento en que se vuelve a llamar a control los procesa, creyendo que uno de los nuevos procesos a muerto, cuando no es así y repitiendo el proceso)y los relanza, guardando la nueva lista.
control(Pids,Mods,Funcs,Args)->
    receive
	{'DOWN',_Ref,process,Pid,normal}->
	    io:format("The process ~w has finished its execution normally~n",[Pid]),
	    {NewPids,NewMods,NewFuncs,NewArgs} = delete(Pid,Pids,Mods,Funcs,Args),
	    control(NewPids,NewMods,NewFuncs,NewArgs);
	{'DOWN',_Ref,process,Pid,Reason} ->
	    io:format("Process ~w crashes.Reason: ~p. I will kill all the processes~n",[Pid,Reason]),
	    Pids2kill=deleteList(Pid,Pids),
	    kill_all(Pids2kill),
	    receive_all_confirmation(Pids2kill),
	    NewPids = spawn_all(Mods,Funcs,Args),
	    monitorizar(NewPids),
	    control(NewPids,Mods,Funcs,Args)
    end.

%Función que dado un elemento y una lista elimina ese elemento de la misma.
deleteList(_Pid,[])->
    [];
deleteList(Pid,[P|Pids]) ->
    case Pid == P of
	true ->
	    deleteList(Pid,Pids);
	false ->
	    [P|deleteList(Pid,Pids)]
    end.

%% Función que dado un Pid elimina su información de las listas.
delete(_Pid,[],[],[],[])->
    {[],[],[],[]};


delete(Pid,[P|Pids],[M|Mods],[F|Funcs],[A|Args])->
    case Pid == P of
	true->
	    delete(Pid,Pids,Mods,Funcs,Args);
	false ->
	    {NP,NM,NF,NA} = delete(Pid,Pids,Mods,Funcs,Args),
	    {[P|NP],[M|NM],[F|NF],[A|NA]}
    end.

%%Función que envía la señal de kill a todos los procesos que recibe en la lista Pids
kill_all(Pids)->
    [exit(P,kill)|| P<-Pids].

%%Función que espera a la confirmación de la recepción del mensaje de todos los procesos restantes para evitar un bucle infinito. Recibe en una lista los Pids de los procesos 
%%de los que espera la recepción del mensaje. En el momento en que hayamos confirmado la muerte de todos, podremos volver a relanzarlos.
receive_all_confirmation(Pids)->
    receive
	{'DOWN',_Ref,process,Pid,_Reason}->
	    io:format("Process ~w killed~n",[Pid]),
	    Pids2kill = deleteList(Pid,Pids),
	    case Pids2kill==[] of
		true->
		    io:format("Every processes killed~n");
		false ->
		    receive_all_confirmation(Pids2kill)
	    end
    end.

%%Funciones utilizadas para comprobar el funcionamiento. Se llama a la función test() que se encarga de lanzar el método my_spawn_all con las funciones test1,test2,test3 y test4 con sus correspondientes 
%% argumentos devolviendo una lista con los Pids de los mismos. Tras ello desde la shell se puede "jugar" con los procesos enviandoles el mensaje Pid!stop para causar su finalización de forma normal 
%% (y no relanzarlo) o con exit(lists:nth(Numero,Pids),kill) para causar la muerte de uno de los procesos y que se relance. 

%% Esta acción solo se puede ejecutar una vez por proceso ya que al relanzarlo perdemos su referencia.
test1(Ts)->
    receive
	stop->
	    void
    after Ts-> 
	    io:format("Primer proceso~n"),
	    test1(Ts)
    end.

test2(Ts)->
    receive
	stop->
	    void
    after Ts->
	    io:format("Segundo proceso~n"),
	    test2(Ts)
    end.

test3(Ts)->
    receive
	stop->
	    void
    after Ts->
	    io:format("Tercer proceso~n"),
	    test3(Ts)
    end.

test4()->
    receive
	stop->
	    void
    after 7000->
	    io:format("Cuarto proceso~n"),
	    test4()
    end.
%%Función utilizada para comprobar el funcionamiento.
%% Lanza 4 procesos que escribiran por pantalla cada 2,3, 5 y 7 segundos respectivamente.
%% Tras ello el proceso que lanza test se duerme.
%% Al despertarse 
test()->
    my_spawn_all_kill([spawn5,spawn5,spawn5,spawn5],[test1,test2,test3,test4],[[2000],[3000],[5000],[]]).


