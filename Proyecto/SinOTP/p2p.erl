-module(p2p).
-export([start/1,start_master/1]).
-include("config.hrl").

start(Path)->
    spawn(?MODULE,start_master,[Path]).
%    register(master,spawn(?MODULE,start_master,[Path])).

%%Path es el camino absoluto o relativo a la carpeta principal
%% Función principal que lee el archivo de configuración y crea el registro a partir de los datos leidos.
%% Si no se ha producido ningún error en la generación del mismo crea el servidor y los usuarios.

start_master(Path)->
    List=read_config(Path),
    Record = create_record_from_list(List),
    case Record of
	error->
	    io:format("There are some error in the configuration file.~n");
	_ -> PidServer= server:create_server(),
%	     io:format("Voy a crear los usuarios"),
	     create_users(Path,Record,PidServer),
	     io:format("El id del servidor es: ~p .~n",[PidServer]),
	     PidServer
	end.

%%Función que recibe el path a la carpeta principal y abre el archivo config.txt.
%% En caso de que no se produzca ningún error en la apertura del mismo lee todas las líneas y las devuelve en una lista.
    
read_config(Path)->
    Name=filename:join(Path,"config.txt"),
    Result=file:open(Name,[read]),
    case Result of
	{ok,Fid}->L = read_lines(Fid,[]),
		  file:close(Fid),
		  L;
	{error,Reason} ->io:format("The file config can not be opened. REASON: ~p .~n",[Reason])
    end.

%% Función que recibe el identificador del fichero a leer y una lista donde se almacenarán los datos leidos.
read_lines(Fid,L)->
    case io:get_line(Fid,"") of
	eof->
	    lists:reverse(L);
	error -> io:format("There is an error~n.");
	X -> List=string:tokens(X," "),
	     [Field,Value] = lists:map(fun(Element)->
					       string:strip(Element)
				       end,List),
	     FieldA = list_to_atom(Field),
	     {ValueInt,_} = string:to_integer(Value),
	     read_lines(Fid,[{FieldA,ValueInt}|L])
    end.

%% Función que recibe los los datos leidos en el fichero config.txt y crea el registro.
%% Si no se produce el ajuste de patrones será a la existencia de un error en el archivo de configuración y no se generará el registro.
create_record_from_list([{'NUsuarios',Val1},{'MaxSeed',Val2},{'MaxDownload',Val3},{'TimeOutWaitingSeeder',Val4},{'TimeOutWaitingPetition',Val5},{'ChunkSize',Val6}])->
    #configuracion{'NUsuarios'=Val1,'MaxSeed'=Val2,'MaxDownload'=Val3,'TimeOutWaitingSeeder'=Val4,'TimeOutWaitingPetition'=Val5,'ChunkSize'=Val6};

create_record_from_list(_List) ->
    error.

%% Función que recibe el path a la carpeta principal, el registro con los datos de la configuración y el identificador del servidor y crea todos los usuarios.
create_users(Path,Record,PidServer)->
%    io:format("Argumentos de create_users ~p ~p ~p~n",[Path,Record,PidServer]),
    [userp2p:create_user(Path,Record,PidServer,I) || I<-lists:seq(0,Record#configuracion.'NUsuarios'-1)].
%    [io:format("Usuario ~p creado.~n",[I]) || I<-lists:seq(0,Record#configuracion.'NUsuarios'-1)].
