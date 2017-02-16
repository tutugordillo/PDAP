-module(download).
-export([create_download/5,start_download/5]).

%%Función que crea el proceso download. Recibe el path a la carpeta de usuario (Path), el registro de configuración (Record), el nombre del fichero (Filename),
%% el Pid del usuario (PidUser) y el Pid del servidor (PidServer).

create_download(Path,Record,Filename,PidUser,PidServer)->
    spawn(?MODULE,start_download,[Path,Record,Filename,PidUser,PidServer]).

%% Función que comprueba que exista el directorio asociado al fichero Filename (check_folder) y si no es así la crea. Tras ello envía al servidor un mensaje para comenzar a pedir semillas.
%% Tras ello comienza con la recepción y tratamiento de mensajes.

start_download(Path,Record,Filename,PidUser,PidServer)->
    Result = check_folder(Path,Filename),
    case Result of
	ok->
	    PidServer!{self(),start,Filename},
	    loop_download(PidUser,PidServer,Filename,Path,Record,[]);
	_->io:format("There is an error while creating the directory bound to file : ~p.~n",[Filename])
    end.


%% Bucle de tratamiento de mensajes.

loop_download(PidUser,PidServer,Filename,Path,Record,Seeds)->
    receive
	{PidSeed,iseedyou,Filename,NChunks}->
%	    io:format("Recibo mensaje iseedyou de ~p~n",[PidSeed]),
	    PidDownloadSeed=downloadseed:create_downloadfromseed(self(),PidSeed,Path,Record,Filename,NChunks),
	    loop_download(PidUser,PidServer,Filename,Path,Record,[PidDownloadSeed|Seeds]);
	
	{_PidDownloadFromSeed,completed}->
%	    io:format("Recibo mensaje completed de ~p~n",[PidDownloadFromSeed]),
	    concat_and_delete_chunks(Path,Filename),
%	    io:format("Seeds: ~p~n",[Seeds]),
	    [P!close || P<-Seeds],
	    PidUser!{self(),enddownload},
	    io:format("The process download ~p finished its execution.~n",[self()]);

	{PidDownloadFromSeed,timeout}->
	    NewSeeds=lists:delete(PidDownloadFromSeed,Seeds),
	    case NewSeeds of
		[]->PidServer!{self(),start,Filename};
		_->ok
	    end,
	    loop_download(PidUser,PidServer,Filename,Path,Record,NewSeeds);

	close->
	    [P!close||P<-Seeds]
    end.

%%Path tiene la ruta a la carpeta del usuario.
%% Crea o comprueba la existencia del directorio asociado a filename.

check_folder(Path,Filename)->
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
%	    io:format("Nuemro de ficheros: ~p.~n",[NumFiles]),
	    Completed_FileId=create_file(Path,Filename),
	    [process_chunk(Download_Path,Id,Completed_FileId)|| Id<-lists:seq(1,NumFiles)],
%	    io:format("Ficheros: ~p.~n", [AllFilenames]),
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
    {ok,FileId}=file:open(Chunk_name,[read]),
    Elements=read_lines(FileId,[]),
    [io:format(Completed_FileId,"~s",[X])||X<-Elements],
    file:close(Chunk_name).
    

%% Almacena líena a línea en la Lista L lo leido del archivo cuyo identificador es Fid.

read_lines(Fid,L)->
    case io:get_line(Fid,"") of
	eof->
	    lists:reverse(L);
	error -> io:format("There is an error~n.");
	X -> read_lines(Fid,[X|L])
    end.
