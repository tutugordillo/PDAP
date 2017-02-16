-module(downloadseed).
-export([create_downloadfromseed/6,start_downloadfromseed/6]).
-include("config.hrl").

%%Función que crea el proceso downloadfromseed. Recibe el identificador del proceso download que lo crea (PidDownload), el identificador del proceso seed que nos sirve (PidSeed),
%%el path a la carpeta de usuario (Path), el registro de configuración (Record), el nombre del fichero (Filename) y el número de chunks (NChunks).

create_downloadfromseed(PidDownload,PidSeed,Path,Record,Filename,NChunks)->
   spawn(?MODULE,start_downloadfromseed,[PidDownload,PidSeed,Path,Record,Filename,NChunks]).


%% En primer lugar comprueba si el número de chunks descargados ya es el número de chunks totales (check_chunks).
%% En caso afirmativo envía al proceso download completed y en caso contrario pregunta por un chunk y comienza el bucle.

start_downloadfromseed(PidDownload,PidSeed,Path,Record,Filename,NChunks)->
    Filename_Completed = Filename++"_dir/",
    Filename_dir = Path++"downloading/"++Filename_Completed,
    {AllFiles,NumFilesEquals}=check_chunks(Filename_dir,NChunks),
    case NumFilesEquals of
	true->
	    PidDownload!{self(),completed};
	_ ->Num=get_random_num("chunk_",NChunks,AllFiles),
	    PidSeed!{self(),askchunk,Num,Filename},
	    loop_downloadfromseed(Filename,Filename_dir,NChunks,PidDownload,PidSeed,Record)
    end.

%% Bucle de tratamiento y recepción de mensajes. Recibe el nombre del fichero (Filename), el directorio donde se encuentran los chunks del mismo (Filename_dir), el número de chunks totales (Nchunks),
%% el identificador dle proceso Download y Seed (PidDownload y PidSeed) y el registro con la configuración (Record).
loop_downloadfromseed(Filename,Filename_dir,NChunks,PidDownload,PidSeed,Record)->
    receive
	{PidSeed,thechunk,J,Chunk}->
	    create_chunk(Filename_dir,J,Chunk),
	    {AllFiles,NumFilesEquals}=check_chunks(Filename_dir,NChunks),
	    case NumFilesEquals of
		true->
		    PidDownload!{self(),completed};
		_ ->Num=get_random_num("chunk_",NChunks,AllFiles),
		    PidSeed!{self(),askchunk,Num,Filename},
		    loop_downloadfromseed(Filename,Filename_dir,NChunks,PidDownload,PidSeed,Record)
	    end;
	
	close->io:format("The process downloadfromseed ~p finished.~n",[self()])
 
    after Record#configuracion.'TimeOutWaitingPetition'->
	    io:format("The process has exceeded the limit time waiting for a petition.~n"), 
	    PidDownload!{self(),timeout}
    end.
	     
    
%%COmprueba si el número de chunks existentes en Filename_dir coincide con NChunks y devuelve el resultado de la comparación y la lista completa de chunks existentes.
check_chunks(Filename_dir,NChunks)->
    ListFiles=file:list_dir_all(Filename_dir),
    case ListFiles of
	{ok,AllFiles}->
	    NumFiles=length(AllFiles),
	    Equals = NumFiles==NChunks,
	    {AllFiles,Equals};
	{error,Reason}->io:format("Th execution fails due to an error checking the number of chunks. REASON: ~p.~n",[Reason])
    end.

%% Función que crea el fichero asociado al chunk J y escribe en el mismo el contenido de Chunk.
create_chunk(Filename_dir,J,Chunk)->
    Chunk_name = Filename_dir++"chunk_"++integer_to_list(J),
    {ok,ChunkId}=file:open(Chunk_name,[write]),
    io:format(ChunkId,"~s",[Chunk]),
    file:close(ChunkId).


%%Función que genera de forma aleatoria un numero entre 1 y NChunks y comprueba si existe ese chunk o no en AllFiles (lista con todos los chunks existentes).
%% EN caso de que exista genera otro número aleatorio. Si no existe devuelve el número para que sea el proximo chunk a pedir.
get_random_num(Chunk_name,NChunks,AllFiles)->
    Num=random:uniform(NChunks),
    Chunk = Chunk_name++integer_to_list(Num),
    case lists:member(Chunk,AllFiles) of
	true->
	    get_random_num(Chunk_name,NChunks,AllFiles);
	false -> Num
    end.
    
