%% Autor: Pablo Gordillo

-module(otp3).
-export([get_list/0,get_list/1,read_csv/2,parser/1,toInteger/1,table/1,table_from_file/1,query1/0,query2/0,query3/0]).
-include_lib("stdlib/include/ms_transform.hrl").

-define(TABLENAME, clients_table).
%%Ejercicio 1

%%Función que nos devuelve una lista de tuplas con los valores de los clientes existentes en el archivo clientes.txt
get_list()->
    get_list("clientes.txt").

%%Función que devuelve una lista de tuplas correspondiente al archivo pasado como parámetro.
get_list(File)->
    {ok,F}=file:open(File,[read]),
    List = read_csv(F,[]),
    lists:reverse(List).

%%Función que dado el identificador de un fichero lo lee línea a línea hasta que llega al final. Cada línea la parsea. Lo leido se devuelve en la variable L.
read_csv(F,L)->
    case io:get_line(F,"") of
	eof->
	    L;
	error -> io:format("There is an error~n.");
	X -> Tuple = parser(X),
		  read_csv(F,[Tuple|L])
	
    end.

%%Función que parsea cada una de las líneas leidas. Separa los elementos por comas, y procesa la línea leida.
parser(X)->
    List=string:tokens(X,","),
    [Id,N,E,C] = lists:map(fun(Element)->
				E= string:strip(Element),
				toInteger(E)
			end,List),
    {Id,N,E,C}.

%%Función que convierte los strings que representan valores numéricos en enteros. 
toInteger(E)->
    case string:to_integer(E) of
	{Elem,[]}->
	    Elem;
	_ -> string:strip(E,both,$\n)
    end.

%%Ejercicio 2
%%Función que recibe una lista de tublas como parametro y genera una tabla ets que contenga dichas tuplas.
table(Tuples)->
    T = ets:new(?TABLENAME, [set,public, named_table]),
    ets:insert(T,Tuples),
    T.

%%Interfaz de usuario que permite leer y procesar el archivo pasado como parámetro creando la correspondiente tabla.(Ejercicio 1 y 2 a la vez).
table_from_file(File)->
    T= get_list(File),
    table(T),
    io:format("The table is ready!~n"),
    T. 

%%Ejercicio 3
%%Ciudad de residencia del usuario con identificador 3.
query1()->
    ets:match(?TABLENAME,{3,'_','_','$1'}).
	    
%%Nombres y apellidos de los residentes en Madrid cuyo nombre comience por C.
query2()->
   F= ets:fun2ms(
      fun ({_,[N|Rest],_,"Madrid"}) when [N]=="C"->
	      [N|Rest]
      end),
     ets:select(?TABLENAME, F).

%%Nombres y apellidos de los usuarios que tengan entre 20 y 30 años.
query3()->
    F= ets:fun2ms(
      fun ({_,Name,Age,_}) when (Age>=20) and (Age=<30)->
	      Name
      end),
     ets:select(?TABLENAME, F).
