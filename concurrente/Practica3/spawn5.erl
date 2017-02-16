%Autor:Pablo Gordillo

-module(spawn5).
-export([my_spawn_all/3,spawn_all/3,monitorizar/1,control/4,relaunch/5,replace/3,test1/1,test2/1,test3/1,test4/0,test/0]).

%% Función principal que recibe una lista de Modulos, Funciones y Argumentos respectivamente.
%% Esta función comprueba que la longitud de las tres listas sea la misma,es decir,que todos los argumentos para cada una de las funciones que se va a lanzar estén definidos. Notese que la lista Argumentos será una lista de listas, ya que contendrá los posibles argumentos para cada una de las funciones (ver llamada a test al final).

%% Esta función lanza un proceso por cada una de las funciones con el método usual y tras ello lanza un nuevo proceso que se encargará de monitorizarlos a todos.
%% Este nuevo proceso tiene una llamada a monitorizar, que recibe la lista de Pids y crea un monitor para cada proceso y llama a control con la lista de Pids y las listas anteriores.

my_spawn_all(Mods,Funcs,Args)->
    case (length(Mods)==length(Funcs)) and (length(Funcs)== length(Args)) of
	true->
	    Pids= spawn_all(Mods,Funcs,Args),
	    spawn(fun()->monitorizar(Pids),control(Pids,Mods,Funcs,Args) end),
	    Pids;
	false -> io:format("Incorrect number of arguments~n")
    end.

%%Función que recibe una lista de Pids y crea un monitor para cada uno de esos procesos.
monitorizar(Pids)->
    [erlang:monitor(process,P)||P<-Pids].

%%Función que monitoriza los procesos. En caso de que el proceso finalice de forma normal, lo indica y sigue controlando al resto.(Mejora: Borrar los datos de este proceso de las listas).
%% En caso de que finalice de forma anormal, lo indica, lo relanza, sustituye el nuevo Pid en la lista de Pids y sigue con el control.
control(Pids,Mods,Funcs,Args)->
    receive
	{'DOWN',_Ref,process,Pid,normal}->
	    io:format("The process ~w has finished its execution normally~n",[Pid]),
	    control(Pids,Mods,Funcs,Args);
	{'DOWN',_Ref,process,Pid,Reason} ->
	    io:format("Process ~w crashes.Reason: ~p It is relaunched~n",[Pid,Reason]),
	    NewPid=relaunch(Pid,Pids,Mods,Funcs,Args),
	    erlang:monitor(process,NewPid),
	    NewPids=replace(Pid,NewPid,Pids),
	    control(NewPids,Mods,Funcs,Args)
    end.

%%Función que recorre las listas y lanza los procesos para cada una de las funciones
spawn_all([],[],[])->
    [];
spawn_all([M|Mods],[F|Funcs],[A|Args]) ->
    Pid = spawn(M,F,A),
    [Pid|spawn_all(Mods,Funcs,Args)].

%Función que relanza un proceso. Va recorriendo las listas buscando los datos del proceso Pid. Cuando se encuentran se relanza el nuevo proceso devolviendo su nuevo Pid. En caso de no encontrarlo
%imprime un mensaje avisando de que ese proceso no estaba monitorizado.
relaunch(Pid,[],[],[],[])->
    io:format("Process identifier ~w is not a member of the Pids list",[Pid]);
 
relaunch(Pid,[Pid|_Pids],[M|_Mods],[F|_Funcs],[A|_Args])->
     spawn(M,F,A);

relaunch(Pid,[_P|Pids],[_M|Mods],[_F|Funcs],[_A|Args])->
    relaunch(Pid,Pids,Mods,Funcs,Args).

%Función que se encarga de reemplazar en la lista Pids el Pid por en luevo valor NewPid
replace(_Pid,_NewPid,[])->
    [];
replace(Pid,NewPid,[P|Pids])->
    case Pid==P of
	true->
	    [NewPid|replace(Pid,NewPid,Pids)];
	false->
	    [P|replace(Pid,NewPid,Pids)]
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
%% Lanza 3 procesos que escribiran por pantalla cada 2,3 y 5 segundos respectivamente.
%% Tras ello el proceso que lanza test se duerme.
%% Al despertarse 
test()->
    my_spawn_all([spawn5,spawn5,spawn5,spawn5],[test1,test2,test3,test4],[[2000],[3000],[5000],[]]).

