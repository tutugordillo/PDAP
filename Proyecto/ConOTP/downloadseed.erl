-module(downloadseed).
-export([create_downloadfromseed/6,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("config.hrl").
-behaviour(gen_server).

%%Función que crea el proceso downloadfromseed. Recibe el identificador del proceso download que lo crea (PidDownload), el identificador del proceso seed que nos sirve (PidSeed),
%%el path a la carpeta de usuario (Path), el registro de configuración (Record), el nombre del fichero (Filename) y el número de chunks (NChunks).
create_downloadfromseed(PidDownload,PidSeed,Path,Record,Filename,NChunks)->
   {ok,PidDownloadSeed}=gen_server:start(?MODULE,{PidDownload,PidSeed,Path,Record,Filename,NChunks},[]),
    PidDownloadSeed.


%% En primer lugar comprueba si el número de chunks descargados ya es el número de chunks totales (check_chunks).
%% En caso afirmativo envía al proceso download completed y en caso contrario pregunta por un chunk y comienza el bucle.
%% Por la misma razón que en el caso del lector se envía un cast en lugar de un call.
%% El estado está formado por el nombre del fichero, el path al directorio correspondiente al mismo, el número de chunks, y los identificadores del proceso download y seed además del timeout necesario para las peticiones.
%% En la inicialización se inlcuye la opción de timeout en caso de que no se reciba una petición en el tiempo especificado en el registro de configuración.
init({PidDownload,PidSeed,Path,Record,Filename,NChunks})->
    Filename_Completed = Filename++"_dir/",
    Filename_dir = Path++"downloading/"++Filename_Completed,
    {AllFiles,NumFilesEquals}=check_chunks(Filename_dir,NChunks),
    case NumFilesEquals of
	true->
	    gen_server:cast(PidDownload,{self(),completed}),
	    {stop,normal};
	_ ->Num=get_random_num("chunk_",NChunks,AllFiles),
	    gen_server:cast(PidSeed,{self(),askchunk,Num,Filename}),
	    {ok,{Filename,Filename_dir,NChunks,PidDownload,PidSeed,Record#configuracion.'TimeOutWaitingPetition'},Record#configuracion.'TimeOutWaitingPetition'}
    end.

%%No se reciben mensajes call
handle_call(_,_From,State)->
    io:format("Message call is not supported by the downloadseed process.~n"),
    {noreply,State}.

%% Mensaje thechunk. Crea el fichero correspondiente y almacena el chunk en el mismo.
%% Tras ello finaliza su ejecución o sigue pidiendo más chunks en caso de que no los tenga todos.
%% EL estado no se ve modificado.
handle_cast({PidSeed,thechunk,J,Chunk},{Filename,Filename_dir,NChunks,PidDownload,PidSeed,Timeout})->
    create_chunk(Filename_dir,J,Chunk),
    {AllFiles,NumFilesEquals}=check_chunks(Filename_dir,NChunks),
    case NumFilesEquals of
	true->
	    gen_server:cast(PidDownload,{self(),completed}),
	    {stop,normal,{Filename,Filename_dir,NChunks,PidDownload,PidSeed,Timeout}};
	_ ->Num=get_random_num("chunk_",NChunks,AllFiles),
	    gen_server:cast(PidSeed,{self(),askchunk,Num,Filename}),
	    {noreply,{Filename,Filename_dir,NChunks,PidDownload,PidSeed,Timeout},Timeout}
    end;

%% Mensaje close. Finaliza la ejecución del proceso.
handle_cast(close,State)->
    {stop,normal,State}.

%% Tratamiento en caso de que se obtenga un timeout. Informa de ello, envía un mensaje al download informando de ello y finaliza la ejecución del proceso.
handle_info(timeout,{Filename,Filename_dir,NChunks,PidDownload,PidSeed,Timeout})->
    io:format("The process has exceeded the limit time waiting for a petition.~n"), 
    gen_server:cast(PidDownload,{self(),timeout}),
    {stop,normal,{Filename,Filename_dir,NChunks,PidDownload,PidSeed,Timeout}};

handle_info(_,State)->
    io:format("Message is not supported by the downloadseed process.~n"),
    {noreply,State}.

%% Informa de la finalización del proceso.
terminate(_,_)->
    io:format("The process downloadfromseed ~p finished.~n",[self()]).

code_change(_PrevoiusVersion,State,_Extra)->
    {ok,State}.
	     
%%Comprueba si el número de chunks existentes en Filename_dir coincide con NChunks y devuelve el resultado de la comparación y la lista completa de chunks existentes.
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
%% En caso de que exista genera otro número aleatorio. Si no existe devuelve el número para que sea el proximo chunk a pedir.
get_random_num(Chunk_name,NChunks,AllFiles)->
    Num=random:uniform(NChunks),
    Chunk = Chunk_name++integer_to_list(Num),
    case lists:member(Chunk,AllFiles) of
	true->
	    get_random_num(Chunk_name,NChunks,AllFiles);
	false -> Num
    end.
    
