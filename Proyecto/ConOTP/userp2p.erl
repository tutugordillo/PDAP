-module(userp2p).
-export([create_user/4,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("config.hrl").
-behaviour(gen_server).

%%Función que recibe el path a la carpeta principal, el registro de configuración, el identificador del servidor y el número de indentificador del usuario y crea el proceso.
create_user(Path,Record,PidServer,Id)->
    {ok,PidUser}=gen_server:start(?MODULE,{Path,Record,PidServer,Id},[]),
    PidUser.

%% Función de inicialización del usuario.Registra al usuario en el servidor y crea el proceso lector.
%% El estado esta formado por el path del usuario, el registro de configuración,el Pid del servidor, el PID del lector, y dos listas vacias donde almacenará
%% los procesos download y seed creados durante la ejecución.
init({Path,Record,PidServer,Id})->
    register_user(PidServer,Id),
    {User_Path,Download_Path}=create_dir(Path,Id),
    check_dir(User_Path,Download_Path),
    PidLector=lector:create_lector(User_Path,self()),
    {ok,{User_Path,Record,PidServer,PidLector,[],[]}}.

%%Función que registra a un usuario y espera la recepción del mensaje de confirmación.
register_user(PidServer,Id)->
    X=gen_server:call(PidServer,{self(),register}),
    case X of
	{_,ack}->
	    io:format("User ~p registered.~n",[Id]);
	_ -> 
	    io:format("Error while creating the user ~p.~n",[Id])
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

%% No se recibe ningún mensaje de tipo call.
handle_call(_Message,_From,State)->
    io:format("Message call is not supported by the user.~n"),
    {noreply,State}.

%% Recepción mensaje download. Crea un proceso download en caso de que sea posible.
%% En caso de que se cree el proceso download el estado se ve modificado añadiendo su identificador a la lista.
%% En otro caso se mantiene el mismo estado.
handle_cast({PidLector,download,Filename},{Path,Record,PidServer,PidLector,Downloading_list,Seed_list})->
    case length(Downloading_list)== Record#configuracion.'MaxDownload' of
	true->
	    gen_server:cast(PidLector,{self(),rejected,Filename}),
	    {noreply,{Path,Record,PidServer,PidLector,Downloading_list,Seed_list}};
	_ ->
	    IsFile=check_file(Path,Filename),
	    case IsFile of
		false->
		    PidDownload = download:create_download(Path,Record,Filename,self(),PidServer),
		    NewDownloadList = [PidDownload|Downloading_list];
		_-> NewDownloadList = Downloading_list
	    end,
	    gen_server:cast(PidLector,{self(),accepted,Filename}),
	    {noreply,{Path,Record,PidServer,PidLector,NewDownloadList,Seed_list}}
    end;

%% Recepción mensaje seed. Crea un proceso seed en caso de que sea posible.
%% En caso de que se cree el proceso seed el estado se ve modificado añadiendo su identificador a la lista.
%% En otro caso se mantiene el mismo estado.
handle_cast({PidDownload,seed,Filename},{Path,Record,PidServer,PidLector,Downloading_list,Seed_list}) ->
    case length(Seed_list)==Record#configuracion.'MaxSeed' of
	true->
	    {noreply,{Path,Record,PidServer,PidLector,Downloading_list,Seed_list}};
	_ -> R=check_file(Path,Filename),
	     case R of
		 true-> 
		     PidSeed = seed:create_seed(Path,Record,Filename,self(),PidDownload),
		     NewSeed_list=[PidSeed|Seed_list];
		 _ -> NewSeed_list = Seed_list
	     end,
	     {noreply,{Path,Record,PidServer,PidLector,Downloading_list,NewSeed_list}}
    end;

%%Recepción mensaje endseed. Se elimina el identificador del proceso de la lista y por tanto se modifica el estado.
handle_cast({PidSeed,endseed},{Path,Record,PidServer,PidLector,Downloading_list,Seed_list}) ->
    io:format("Seed finished ~p.~n",[PidSeed]),
    NewSeed_list=lists:delete(PidSeed,Seed_list),
    {noreply,{Path,Record,PidServer,PidLector,Downloading_list,NewSeed_list}};

%%Recepción mensaje enddownload. Se elimina el identificador del proceso de la lista y por tanto se modifica el estado.
handle_cast({PidDownload,enddownload},{Path,Record,PidServer,PidLector,Downloading_list,Seed_list})->
    io:format("Download finished~p.~n",[PidDownload]),
    NewDownloading_list=lists:delete(PidDownload,Downloading_list),
    {noreply,{Path,Record,PidServer,PidLector,NewDownloading_list,Seed_list}};

%%Recepción mensaje close. Reenvía el mensaje al lector y a los procesos download y seed.
%% El estado no se ve modificado.
handle_cast(close,{Path,Record,PidServer,PidLector,Downloading_list,Seed_list})->
    [gen_server:cast(P,close) || P<-Downloading_list],
    [gen_server:cast(P,close) || P<-Seed_list],
    gen_server:cast(PidLector,close),
    {stop,normal,{Path,Record,PidServer,PidLector,Downloading_list,Seed_list}}.

handle_info(_Message,State)->
    io:format("Message is not supported by the user process.~n"),
    {noreply,State}.

%%Informa de la finalización del proceso.
terminate(_Reason,_State)->
    io:format("The user ~p has finished.~n",[self()]).

code_change(_PreviousVersion,State,_Extra)->
    {ok,State}.
