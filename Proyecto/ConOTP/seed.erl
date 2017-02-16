-module(seed).
-export([create_seed/5,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("config.hrl").
-behaviour(gen_server).

%%Función que crea el proceso seed. Recibe el path a la carpeta de usuario (Path), el registro de configuración (Record), el nombre del fichero que quiere (Filename),
%% y el identificador tanto del usuario como del proceso download que lo pide (PidUser y PidDownload).

create_seed(Path,Record,Filename,PidUser,PidDownload)->
    {ok,PidSeed}=gen_server:start(?MODULE,{Path,Record,Filename,PidUser,PidDownload},[]),
    PidSeed.

%% En primer lugar calcula el número de chunks que tiene el fichero Filename.
%% El estado estará formado por el Pid del usuario, la lista con los chunks correspondientes al fichero y el timeout correspondiente.
%%Se inicializa con la opción de timeout para que se contemple desde el método init.

init({Path,Record,Filename,PidUser,PidDownload})->
    NChunks = get_NumChunks(Path,Record,Filename),
    ListChunks = get_chunks(Path,Filename,Record),
    gen_server:cast(PidDownload,{self(),iseedyou,Filename,NChunks}),
    {ok,{PidUser,ListChunks,Record#configuracion.'TimeOutWaitingPetition'},Record#configuracion.'TimeOutWaitingPetition'}.

%% Accede al fichero Filename en el directorio completed de Path, calcula su tamaño y lo divide entre el tamaño del chunk.
%% Devuelve ese resultado.

get_NumChunks(Path,Record,Filename)->
    Completed_Filename = Path++"completed/"++Filename,
    File_size = filelib:file_size(Completed_Filename),
    Chunk_size = Record#configuracion.'ChunkSize',
    (File_size div Chunk_size)+1.

%% Función que recibe el nombre del fichero, el path a la carpeta del usuario y el registro de configuración y almacena en una lista L que devuelve cada uno de los chunks correspondientes al fichero.
%% Esto es: en la posición 1 de la lista estará el primer chunk del fichero...hasta la posición n.
get_chunks(Path,Filename,Record)->
    Chunk_size = Record#configuracion.'ChunkSize',
    Completed_Filename = Path++"completed/"++Filename,
    {ok,Fid}=file:open(Completed_Filename,[read]),
    L=all_chunks(Fid,Chunk_size,[]),
    file:close(Fid),
    L.
    
%%Función que va leyendo del archivo identificado con Fid, de Chunk_size en Chunk_size hasta llegar a eof y lo almacena en la lista List.
all_chunks(Fid,Chunk_size,List)->
    Element=file:read(Fid,Chunk_size),
    case Element of
	eof->
	    lists:reverse(List);
	{ok,X} -> all_chunks(Fid,Chunk_size,[X|List])
    end.

%%No se reciben mensajes call.
handle_call(_,_,State)->
    io:format("Message call is not supported by the seed process.~n"),
    {noreply,State}.

%% Recepción mensaje askchunk. Enviamos el chunk correspondiente a la posición indicada.
%% No se modifica el estado.
%% Se incluye en el valor devuelto el Timeout existente en el estado para provocar que el proceso finalice en caso de que no reciba más peticiones.
handle_cast({PidSeed,askchunk,J,_Filename},{PidUser,List,TimeOut})->
    Chunk = lists:nth(J,List),
    gen_server:cast(PidSeed,{self(),thechunk,J,Chunk}),
    {noreply,{PidUser,List,TimeOut},TimeOut};

%% Recepción mensaje close. Se finaliza el proceso.
handle_cast(close,Status)->
    {stop,normal,Status}.

%% Recepción mensaje relacionado con el timeout. Se envía el mensaje endseed al usuario y finaliza la ejecución.
handle_info(timeout,{PidUser,List,Timeout})->
    gen_server:cast(PidUser,{self(),endseed}),
    {stop,normal,{PidUser,List,Timeout}};

handle_info(_,Status)->
    io:format("Message is not supported by the seed process.~n"),
    {noreply,Status}.

%% Informa sobre la finalización del proceso.
terminate(_Reason,_Status)->
    io:format("The process Seed ~p has finised its execution.~n",[self()]).

code_change(_PreviousVersion,Status,_Extra)->
    {ok,Status}.

	    
