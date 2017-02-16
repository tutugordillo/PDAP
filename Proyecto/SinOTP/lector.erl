-module(lector).
-export([create_lector/2,start_lector/2]).


%%Función que crea el proceso lector.
%% Recibe el path a la carpeta del usuario y el Pid del mismo.

create_lector(Path,PidUser)->
    spawn(?MODULE,start_lector,[Path,PidUser]).

%%Comienza la ejecución del proceso lector, obteniendo la primera línea del fichero files.txt (process_line).
%% En caso de que no se haya producido ningún error se la envía al usuario y comienza el bucle de tratamiento de mensajes.

start_lector(Path,PidUser)->
    {Fid,Filename}=process_line(Path),
    case Filename of
	eof->ok;
	_->
	    PidUser!{self(),download,Filename}
    end,
    loop_lector(Path,PidUser,Fid).

%%Bucle de recepción y tratamiento de mensajes.
%% Recibe el path a la carpeta de usuario (Path), el identificador del mismo (PidUser) y el identificador del fichero files.txt (FileId).

loop_lector(Path,PidUser,FileId)->
    receive
	{PidUser,accepted,_Filename}->
	    NewFilename=read_line(FileId),
	    case NewFilename of
		eof-> io:format("Lector ~p finished its function.~n",[self()]);
		_->
		    PidUser!{self(),download,NewFilename}
	    end,
	    loop_lector(Path,PidUser,FileId);
	{PidUser,rejected,Filename} ->
	    PidUser!{self(),download,Filename},
	    loop_lector(Path,PidUser,FileId);
	close ->
	    io:format("Lector ~p is closed.~n",[self()])
    end.

%% Función que recibe el path a la carpeta de usuario, abre en modo lectura el fichero files.txt
%% lee una línea del mismo, la procesa (elimina el salto de línea final) y la devuelve junto al identificador del fichero.

process_line(Path)->
    Filename=filename:join(Path,"files.txt"),
    Result = file:open(Filename,[read]),
    case Result of
	{ok,Fid}->L = read_line(Fid),
		  {Fid,L};
	{error,enoent} -> {ok,F1Id}=file:open(Filename,[write]),
			  file:close(F1Id),
			  {ok,eof};
	{error,Reason} -> io:format("The file config can not be opened. REASON: ~p .~n",[Reason])
    end.

%% Función que dado el identificador de un fichero lee una línea del mismo, elimina el salto de línea final y la devuelve.

read_line(Fid)->
    case io:get_line(Fid,"") of
	eof->
	    eof;
	
	error -> io:format("There is an error.~n"),
		 eof;
	%%String que representa el salto de línea.
	[10]->read_line(Fid);
	
	X ->io:format("Linea Obtenida: ~s~n",[X]), 
	    string:sub_string(X,1,string:len(X)-1)
	     
	     
    end.
