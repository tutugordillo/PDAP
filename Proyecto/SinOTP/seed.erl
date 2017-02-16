-module(seed).
-export([create_seed/5,start_seed/5,all_chunks/3]).
-include("config.hrl").

%%Función que crea el proceso seed. Recibe el path a la carpeta de usuario (Path), el registro de configuración (Record), el nombre del fichero que quiere (Filename),
%% y el identificador tanto del usuario como del proceso download que lo pide (PidUser y PidDownload).

create_seed(Path,Record,Filename,PidUser,PidDownload)->
    spawn(?MODULE,start_seed,[Path,Record,Filename,PidUser,PidDownload]).

%% En primer lugar calcula el número de chunks que tiene el fichero Filename.

start_seed(Path,Record,Filename,PidUser,PidDownload)->
%    io:format("Comienza el seed ~p~n",[self()]),
    NChunks = get_NumChunks(Path,Record,Filename),
    ListChunks = get_chunks(Path,Filename,Record),
    PidDownload!{self(),iseedyou,Filename,NChunks},
    loop_seed(PidUser,ListChunks,Record).

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


%%Bucle de recepción y tratamiento de mensajes. Recibe el identificador del proceso usuario (PidUser) la lista con todos los chunks (List) y el registro de configuración (Record).
loop_seed(PidUser,List,Record)->
    receive
	{PidSeed,askchunk,J,_Filename}->
	    Chunk = lists:nth(J,List),
	    PidSeed!{self(),thechunk,J,Chunk},
	    loop_seed(PidUser,List,Record);

	close->
	    io:format("The process Seed ~p has finised its execution",[self()])
    after Record#configuracion.'TimeOutWaitingPetition'->
	    PidUser!{self(),endseed}
    end.
	    
