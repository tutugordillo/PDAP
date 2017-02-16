-module(download).
-export([create_download/5,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-behaviour(gen_server).

%%Función que crea el proceso download. Recibe el path a la carpeta de usuario (Path), el registro de configuración (Record), el nombre del fichero (Filename),
%% el Pid del usuario (PidUser) y el Pid del servidor (PidServer).

create_download(Path,Record,Filename,PidUser,PidServer)->
    {ok,PidDownload}=gen_server:start(?MODULE,{Path,Record,Filename,PidUser,PidServer},[]),
    PidDownload.

%% Función que comprueba que exista el directorio asociado al fichero Filename (check_folder) y si no es así la crea. Tras ello envía al servidor un mensaje para comenzar a pedir semillas.
%% Tras ello comienza con la recepción y tratamiento de mensajes.
%% El estado está formado por el identificador del usuario, el del servidor, el nombre del fichero, el path a la ruta del usuario, el registro de configuración y una lista donde se 
%% almacenará los identificadores de los procesos DownloadFromSeed creados.

init({Path,Record,Filename,PidUser,PidServer})->
    Result = check_folder(Path,Filename),
    case Result of
	ok->
	    gen_server:cast(PidServer,{self(),start,Filename}),
	    {ok,{PidUser,PidServer,Filename,Path,Record,[]}};
	_->io:format("There is an error while creating the directory bound to file : ~p.~n",[Filename]),
	   {stop,errorinit}
    end.

%% No se reciben mensajes call
handle_call(_Message,_From,State)->
    io:format("Message call is not supported by the download process.~n"),
    {noreply,State}.

%%Mensaje iseedyou. Crea un proceso downloadFromSeed.
%% El estado se ve modificado añadiendo el identificador del proceso creado a la lista.
handle_cast({PidSeed,iseedyou,Filename,NChunks},{PidUser,PidServer,Filename,Path,Record,Seeds})->
%    io:format("Recibo mensaje iseedyou de ~p~n",[PidSeed]),
    PidDownloadSeed=downloadseed:create_downloadfromseed(self(),PidSeed,Path,Record,Filename,NChunks),
    {noreply,{PidUser,PidServer,Filename,Path,Record,[PidDownloadSeed|Seeds]}};

%%Mensaje completed. Se concatenan los chunks y se crea el archivo final. Tras ello finaliza el proceso.
%%El estado no se ve modificado.
handle_cast({_PidDownloadFromSeed,completed},{PidUser,PidServer,Filename,Path,Record,Seeds}) ->
    concat_and_delete_chunks(Path,Filename),
    [gen_server:cast(P,close) || P<-Seeds],
    gen_server:cast(PidUser,{self(),enddownload}),
    {stop,normal,{PidUser,PidServer,Filename,Path,Record,Seeds}};

%% Recibo mensaje timeout. En caso de que no posea más procesos que me sirvan vuelvo a enviar el mensaje para continuar con la descarga.
%% En otro caso no hago nada.
%% El estado no se ve modificado.
handle_cast({PidDownloadFromSeed,timeout},{PidUser,PidServer,Filename,Path,Record,Seeds}) ->
    NewSeeds=lists:delete(PidDownloadFromSeed,Seeds),
    case NewSeeds of
	[]->gen_server:cast(PidServer,{self(),start,Filename});
	_->ok
    end,
    {noreply,{PidUser,PidServer,Filename,Path,Record,NewSeeds}};

%% Mensaje close. Lo reenvía a los procesos downloadfromseed creados y finaliza la ejecución.
handle_cast(close,{PidUser,PidServer,Filename,Path,Record,Seeds}) ->
    [gen_server:cast(P,close) || P<-Seeds],
    {stop,normal,{PidUser,PidServer,Filename,Path,Record,Seeds}}.

handle_info(_,State)->
    io:format("Message is not supported by the download process.~n"),
    {noreply,State}.

%%Informa de la finalización del proceso.
terminate(_,_)->
    io:format("The process download ~p finished its execution.~n",[self()]).

code_change(_PreviousVersion,State,_Extra)->
    {ok,State}.


%%Path tiene la ruta a la carpeta del usuario.
%% Crea o comprueba la existencia del directorio asociado a filename.

check_folder(Path,Filename)->
%    io:format("Filename: ~p~n",[Filename]),
    Dir_name = Filename++"_dir/",
    Download_Path = Path++"downloading/"++Dir_name,
    filelib:ensure_dir(Download_Path).

%% Crea el fichero en la carpeta completed del usuario (create_file) y escribe todos los chunks en orden (process_chunk).
%% Tras ello elimina todos los ficheros chunk y el directorio asociado al fichero Filename

concat_and_delete_chunks(Path,Filename)->
    Dir_name = Filename++"_dir/",
    Download_Path = Path++"downloading/"++Dir_name,
    case file:list_dir_all(Download_Path) of
	{ok,AllFilenames}->
	    NumFiles = length(AllFilenames),
	    Completed_FileId=create_file(Path,Filename),
	    [process_chunk(Download_Path,Id,Completed_FileId)|| Id<-lists:seq(1,NumFiles)],
	    file:close(Completed_FileId),
	    [file:delete(Download_Path++File)|| File<-AllFilenames],
	    file:del_dir(Download_Path)
	    ;
	{error,Reason} -> io:format("There is an error in the path ~p. REASON: ~p .~n",[Download_Path,Reason])
    end.


%% Crea el fichero Filename en la carpeta completed asociada al usuario y lo abre en modo de escritura devolviendo el identificador del mismo.    

create_file(Path,Filename)->
    Completed_dir = Path++"completed/",
    File = Completed_dir++Filename,
    {ok,Fid}=file:open(File,[write]),
    Fid.

%%Recibe path a la carpeta de descargas del fichero, el identificador del chunk y el identificador del fichero final donde escribir.
%% Abre el fichero asociado al chunk en modo lectura y comienza a leer su contenido (línea a línea) almacenándolo en una lista.
%% Tras ello escribe lo leido en el fichero Completed_FileId.

process_chunk(Download_Path,Id,Completed_FileId)->
    Name = "chunk_"++integer_to_list(Id),
    Chunk_name= Download_Path++Name,
    io:format("Chunkname: ~p~n",[Chunk_name]),
    {ok,FileId}=file:open(Chunk_name,[read]),
    Elements=read_lines(FileId,[]),
    [io:format(Completed_FileId,"~s",[X])||X<-Elements],
    file:close(Chunk_name).
    

%% Almacena líena a línea en la Lista L lo leido del archivo cuyo identificador es Fid.

read_lines(Fid,L)->
    case io:get_line(Fid,"") of
	eof->
	    lists:reverse(L);
	error -> io:format("There is an error.~n");
	X -> read_lines(Fid,[X|L])
    end.
