-module(pract3).
-include_lib("eunit/include/eunit.hrl").
-export([different/3,differentBis/3,morosos/1,triangulo/1,maxLists/2]).
	       
different(F,G,L)->lists:filter(fun(X)-> F(X)/=G(X) end,L).
    
absus(X)->
    case X>=0 of
	true -> X;
	false -> -X
end.

id(X)->
    X.

 different_test()->
     ?assertEqual([-3,-2,-1],different(fun absus/1,fun id/1,lists:seq(-3,10))).

differentBis(F,G,L)-> [X || X<-L, F(X)/=G(X)].
	
differentBis_test()->
    ?assertEqual([-3,-2,-1],different(fun abs/1,fun id/1,lists:seq(-3,10))).

equal(F,G,L)->
    different(F,G,L)==[].

 equal_test()->
     ?assertEqual(true,equal(fun abs/1,fun abs/1,lists:seq(-10,10))).

morosos(L)->
    [A|| {A,B}<-L,B<0].

morosos_test()->
    ?assertEqual([aniceto],morosos([{bertoldo, 500}, {herminia,
cancelada}, {aniceto,-2000}])).
	
triangulo(N)-> [lists:seq(1,A) || A<-lists:seq(1,N)].    

maxLists(LF,LV)->
    [lists:foldl(fun erlang:max/2,0,[G(Y)||G<-LF]) || Y<-LV].
