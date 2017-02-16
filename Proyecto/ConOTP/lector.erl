-module(lector).
-export([create_lector/2,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-behaviour(gen_server).

%%Función que crea el proceso lector.
%% Recibe el path a la carpeta del usuario y el Pid del mismo.

create_lector(Path,PidUser)->
    {ok,PidLector}=gen_server:start(?MODULE,{Path,PidUser},[]),
    PidLector.

%%  Comienza la ejecución del proceso lector, obteniendo la primera línea del fichero files.txt (process_line).
%%  En caso de que no se haya producido ningún error se la envía al usuario y comienza el bucle de tratamiento de mensajes.
%%  Uso cast en lugar de call pese a que se espere una respuesta por parte del usuario porque si no la lógica entera 
%%  del proceso se llevaría a cabo en el call_back init del proceso, y no me parece razonalble (al recibir una de las respuestas mandaría
%%  otro mensaje y ejecutaría el bucle de espera en el init. De igual manera si se sacase y se realizase la llamada tras el método start se
%%  ejecutaría toda la lógica del proceso en él ya que procesaria la respuesta recibida por el call, mandaría un nuevo call en función de la respuesta y procesaría el mensaje en el mismo método...

init({Path,PidUser})->
    {Fid,Filename}=process_line(Path),
    case Filename of
	eof->ok;
	_->
%	    io:format("NewFilename: ~p~n",[Filename]),
	    gen_server:cast(PidUser,{self(),download,Filename}) 
    end,
    {ok,{Path,PidUser,Fid}}.

%% Por la razón expuesta anteriormente no se ha utilizado ningún handle_call.
handle_call(_Message,_From,State)->
    io:format("Message call is not supported by the lector process.~n"),
    {noreply,State}.


%%Recepción de mensaje accepted. Lee una nueva linea del fichero files.txt. Si es eof finaliza, si no envía dicha línea al usuario.
handle_cast({PidUser,accepted,_Filename},{Path,PidUser,FileId})->
    NewFilename=read_line(FileId),
    case NewFilename of
	eof-> io:format("Lector ~p finished its function.~n",[self()]),
	      {stop,normal,{Path,PidUser,FileId}};
	_->
%	    io:format("NewFilename: ~p~n",[NewFilename]),
	    gen_server:cast(PidUser,{self(),download,NewFilename}),
	    {noreply,{Path,PidUser,FileId}}
    end;

%%Recepción mensaje rejected. Reenvía el mismo mensaje al usuario.
handle_cast({PidUser,rejected,Filename},State) ->
    gen_server:cast(PidUser,{self(),download,Filename}),
    {noreply,State};

%%Mensaje close. Finaliza la ejecución del usuario.
handle_cast(close,State) ->
    {stop,normal,State}.

handle_info(_Message,State)->
    io:format("Message is not supported by the lector process.~n"),
    {noreply,State}.

%%Recepción de mensaje de finalización. Informa de la finalzación del proceso.
terminate(_,_)->
    io:format("Lector ~p is closed.~n",[self()]).

code_change(_PreviousVersion,State,_Extra)->
    {ok,State}.


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
	%%En caso de que sea salto de línea sigo leyendo. (Representación de strings como lista de chars. 10 es el ASCII del salto de línea).
	[10]-> read_line(Fid);
	
	X -> string:sub_string(X,1,string:len(X)-1)
	     
    end.
