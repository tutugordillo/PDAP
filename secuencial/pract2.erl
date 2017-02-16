-module(pract2).
-export([impares/1,ack/2,mismoConjunto/2,normal/1,intersección/2,está/2,nNodos/1,mapTree/2,sonMúltiplos/2,h/1]).


%%%% Ejercicio 1 %%%%%

impares([])->[];
impares([X])->[X];
impares([X,_|Xs])->[X|impares(Xs)].

%%%% Ejercicio 2 %%%%%

ack(0,N)-> N+1;
ack(M,0) when M>0 -> ack(M-1,1);
ack(M,N) when M>0,N>0 -> ack(M-1,ack(M,N-1)).

%%%% Ejercicio 3.1 %%%%

mismoConjunto(X,Y) -> contenido(X,Y) andalso contenido(Y,X).

contenido([],_)-> true;
contenido([X|Xs],Y) -> 
		    case lists:member(X,Y) of
		    	 true -> contenido(Xs,Y);
		       	 false -> false
		    end.

%%%% Ejercicio 3.2 %%%%

normal([X])-> [X];
normal([X|Xs])-> 
		 case lists:member(X,Xs) of
		      true -> normal(Xs);
		      false -> [X|normal(Xs)]
		 end.

%%%% Ejercicio 3.3 %%%%

intersección([],_)->[];
intersección([X|Xs],Y)-> case lists:member(X,Y) of
			      true -> R = intersección(Xs,Y),case lists:member(X,R) of
			      	      	   			  true -> R;
								  false-> [X|R]
								  end;
			      false -> intersección(Xs,Y)
			 end.

%%%% Ejercicio 4.1 %%%%

está(_,{})-> false;
está(E,{E,_,_}) -> true;
está(E,{_,I,D}) -> está(E,I) orelse está(E,D).

%%%% Ejercicio 4.2 %%%%

nNodos({})-> 0;
nNodos({_E,I,D})->1+nNodos(I)+nNodos(D).

%%%% Ejercicio 4.3 %%%%

mapTree(_,{})->{};
mapTree(F,{E,I,D})->{F(E),mapTree(F,I),mapTree(F,D)}.

%%%% Ejercicio 5 %%%%

sonMúltiplos(0,_Y) -> true;
sonMúltiplos(_X,0) -> true;
sonMúltiplos(X,Y) -> case X>Y of
		     	  true -> (X rem Y)==0;
			  false -> (Y rem X) ==0
			  end.

%%%% Ejercicio 6 %%%%

h(X) -> fun 
       (Y) when X==0;Y==0 -> true;
       (Y) when X>Y -> false;
       (Y) -> (Y rem X) == 0
       end.