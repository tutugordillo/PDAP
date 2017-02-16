%%Pablo Gordillo Alguacil


-module(pract4).
-export([poker/1,numeroIguales/2,edad/1,vecinos/2,habitantes/2,incluye/2,mapSafe/2]).
-include("pract4.hrl").

poker(#mano{cartas=C})->
    N=length(C),
    case N<4 of
    	true -> false;
    	false -> [H|R]=C, case numeroIguales(H,R)>=4 of
       			      true->true;
    			      false-> Y=#mano{cartas=R},
				      poker(Y)
    			   end	      
    	end.
    

numeroIguales(C,L)->
    T = lists:filter(fun(X)->X#carta.valor=:=C#carta.valor end,L),
    length(T)+1.

edad(#persona{edad=E})->
    E.

vecinos(#persona{ciudad=C1},#persona{ciudad=C2})->
    C1=:=C2.

habitantes(Ps,Ciudad)-> [P || P<-Ps,P#persona.ciudad=:=Ciudad].
			       
incluye(P,Ps)->
    L = esta(P,Ps),
    case L of
	true->
	    Ps;
	false ->[P|Ps] end.

esta(P,PS)->
    X= lists:filter(fun(X)->
		      X#persona.dni =:= P#persona.dni end, PS),

    length(X)>=1.

mapSafe(F,L)->
    [try F(P) catch _:_->error end || P<-L].
    

