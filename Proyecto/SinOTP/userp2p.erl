-module(userp2p).
-export([create_user/4,start_user/4]).
-include("config.hrl").

%%Función que recibe el path a la carpeta principal, el registro de configuración, el identificador del servidor y el número de indentificador del usuario y crea el proceso.

create_user(Path,Record,PidServer,Id)->
    spawn(?MODULE,start_user,[Path,Record,PidServer,Id]).

%%Función que registra al usuario (register_user), crea o asegura la existencia de las carpetas de descarga y de archivos completos (create_dir),
%% comprueba la existencia del archivo files.txt y almacena en el mismo los ficheros a medio descargar(check_dir),
%% crea el proceso lector y comienza el bucle de tratamiento de mensajes.

start_user(Path,Record,PidServer,Id)->
    register_user(PidServer,Id),
    {User_Path,Download_Path}=create_dir(Path,Id),
    check_dir(User_Path,Download_Path),
    PidLector=lector:create_lector(User_Path,self()),
    loop_user(User_Path,Record,PidServer,PidLector,[],[]).

%%Función que registra a un usuario y espera la recepción del mensaje de confirmación.

register_user(PidServer,Id)->
    PidServer!{self(),register},
    receive
	{PidServer,ack}->
	    io:format("Usuario ~p registrado.~n",[Id]),
	    ok
    end.

%%Genera todos los directorios correspondientes al usuario.

create_dir(Path,Id)->
    User_dir = Path++"User_"++integer_to_list(Id)++"/",
    Download = User_dir++"downloading/",
    Completed= User_dir++"completed/",
    filelib:ensure_dir(Download),
    filelib:ensure_dir(Completed),
    {User_dir,Download}.

%% Dado un identificador de fichero coloca el descriptor al final del mismo para que en caso de que 
%% se escriba en el mismo no borre lo ya existente en el mismo.

locate_eof(Fid)->
    case io:get_line(Fid,"") of
	eof->ok;
	_ -> locate_eof(Fid)
    end.

%% Función que dada una lista de directorios Dirs y el identificador de un fichero, escribe en el mismo el nombre de los
%% ficheros asociados a cada uno de los directorios (el nombre del directorio sin el sufijo _dir). 

parser_download_dirs(Dirs,Fid)->
    locate_eof(Fid),
    lists:map(fun(Element)->
		      [Name|_] = string:tokens(Element,"_"),
		      io:format(Fid,"~s~n",[Name])
	      end,Dirs).

%% Función que recibe el path a la carpeta de usuario(User_Path) y a la carpeta de descargas(Download_Path). 
%% Genera una lista con los nombres de los subdirectorios de la carpeta download (ficheros a medio descargar),
%% Crea el archivo files.txt en caso de que no exista y escribe en él el nombre de dichos ficheros.

check_dir(User_Path,Download_Path)->
    List=file:list_dir_all(Download_Path),
    case List of
	{ok,[]}->
	    ok;
	
	{ok,Dirs}->
	    Filename = User_Path++"files.txt",
	    {ok,FId}=file:open(Filename,[read,write]),
	    parser_download_dirs(Dirs,FId),
	    file:close(FId);
	
	{error,Reason} -> io:format("There is an error while checking the download files. REASON:~p .~n",[Reason])
    end.

%%Función que devuelve true si el fichero Filename se encuentra en el directorio de ficheros completos asociado al usuario.

check_file(Path,Filename)->
    Completed = Path++"completed/",
    {ok,List}=file:list_dir_all(Completed),
    lists:member(Filename,List).


%%Bucle de recepción y tratamiento de mensajes.
%% Recibe el path a la carpeta del usuario (Path), el identificador del servidor (PidServer),
%% el identificador del proceso lector generado anteriormente (PidLector), una lista con los procesos download creados (Downloading_list)
%% y una lista con los procesos seed creados (Seed_list).

loop_user(Path,Record,PidServer,PidLector,Downloading_list,Seed_list)->
    receive
	{PidLector,download,Filename}->
	    case length(Downloading_list)== Record#configuracion.'MaxDownload' of
		true->
		    PidLector!{self(),rejected,Filename},
		    loop_user(Path,Record,PidServer,PidLector,Downloading_list,Seed_list);
		_ ->
%		    io:format("Soy ~p y voy a descargar ~s~n",[Id,Filename]),
		    IsFile=check_file(Path,Filename),
	    	    case IsFile of
			false->
			    PidDownload = download:create_download(Path,Record,Filename,self(),PidServer),
			    NewDownloadList = [PidDownload|Downloading_list];
			_->
			    NewDownloadList = Downloading_list
	    	    end,
		    PidLector!{self(),accepted,Filename},
		    loop_user(Path,Record,PidServer,PidLector,NewDownloadList,Seed_list)
	    end;
	
	{PidDownload,seed,Filename}->
	    case length(Seed_list)==Record#configuracion.'MaxSeed' of
		true->
		    loop_user(Path,Record,PidServer,PidLector,Downloading_list,Seed_list);
		_ -> R=check_file(Path,Filename),
		     case R of
			 true-> 
%			     io:format("soy el usuario ~p y tengo el archivo~s~n",[Id,Filename]),
			     PidSeed = seed:create_seed(Path,Record,Filename,self(),PidDownload),
			     NewSeed_list=[PidSeed|Seed_list];
			 _ -> NewSeed_list = Seed_list
		     end,
		     loop_user(Path,Record,PidServer,PidLector,Downloading_list,NewSeed_list)
	    end;
	
	{PidSeed,endseed}->
	    NewSeed_list=lists:delete(PidSeed,Seed_list),
	    io:format("Seed finished.~n"),
	    loop_user(Path,Record,PidServer,PidLector,Downloading_list,NewSeed_list);
	
	{PidDownload,enddownload} ->
	    NewDownloading_list=lists:delete(PidDownload,Downloading_list),
	    io:format("Download finished.~n"),
	    loop_user(Path,Record,PidServer,PidLector,NewDownloading_list,Seed_list);
		
	close->
	    [P!close|| P<-Downloading_list],
	    [P!close|| P<-Seed_list],
	    PidLector!close,
	    io:format("The user ~p has finished.~n",[self()])
			  
    end.


    
