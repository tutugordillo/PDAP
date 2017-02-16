-module(pract4_test).
-include_lib("eunit/include/eunit.hrl").
-include("pract4.hrl").


eje1Mano_test() -> _ = #mano{cartas=[#carta{valor=2,palo=corazones},
                                    #carta{valor=3,palo=rombos},
                                    #carta{valor=as,palo=treboles},
                                    #carta{valor=2,palo=rombos},
                                    #carta{valor=3,palo=picas}       
                                   ] 
                            }.

eje1Valores_test() -> ?assertEqual(?VALORES,[as,2,3,4,5,6,7,8,9,10,j,q,k]).

eje1Persona_test() -> _ = #persona{edad=25,nombre="bertoldo",apellidos="cacaseno",dni=1,calle="C/Jazmin",ciudad="Madrid"}.

pokerSI_test() -> ?assert(pract4:poker(#mano{cartas=[#carta{valor=3,palo=corazones},
                                   #carta{valor=3,palo=rombos},
                                   #carta{valor=as,palo=treboles},
                                   #carta{valor=3,palo=treboles},
                                   #carta{valor=3,palo=picas}       
                                  ] })).

pokerNO_test() -> ?assertNot(pract4:poker(#mano{cartas=[#carta{valor=3,palo=corazones},
                                   #carta{valor=3,palo=rombos},
                                   #carta{valor=as,palo=treboles},
                                   #carta{valor=q,palo=treboles},
                                   #carta{valor=3,palo=picas}       
                                  ] })).

edad_test() -> P = eje1Persona_test(),
              ?assertEqual(pract4:edad(P),25).

p() -> [eje1Persona_test(), #persona{edad=28,nombre="herminia",apellidos="Filón",
                                      dni=2,calle="C/Jazmin",ciudad="Madrid"}].

vecinosSI_test() -> [P1,P2] = p(),
                 ?assert(pract4:vecinos(P1,P2)).

vecinosNO_test() -> [P1,P2] = p(),
                 ?assertNot(pract4:vecinos(P1,P2#persona{ciudad="Lugo"})).

habitantes_test() -> [P1,P2] = p(),     
                     ?assertEqual(pract4:habitantes([P1,
                                    P1#persona{ciudad="Lugo"},P2],"Madrid"),
                                  [#persona{nombre = "bertoldo",apellidos = "cacaseno",
                                   dni = 1,edad = 25,calle = "C/Jazmin",ciudad = "Madrid"},
                                   #persona{nombre = "herminia",apellidos = "Filón",dni = 2,
                                   edad = 28,calle = "C/Jazmin",ciudad = "Madrid"}] ).

incluye_test() -> [P1,P2] = p(), 
                 ?assertEqual(pract4:incluye(P1,[P1,P2]),
[#persona{nombre = "bertoldo",apellidos = "cacaseno",
         dni = 1,edad = 25,calle = "C/Jazmin",ciudad = "Madrid"},
#persona{nombre = "herminia",apellidos = "Filón",dni = 2,
         edad = 28,calle = "C/Jazmin",ciudad = "Madrid"}]),
                 ?assertEqual(pract4:incluye(P1,[P2]),                        
[#persona{nombre = "bertoldo",apellidos = "cacaseno",
         dni = 1,edad = 25,calle = "C/Jazmin",ciudad = "Madrid"},
#persona{nombre = "herminia",apellidos = "Filón",dni = 2,
         edad = 28,calle = "C/Jazmin",ciudad = "Madrid"}] ).

map_test() -> ?assertEqual(pract4:mapSafe(fun(X)->1/X end,[1,2,3,4,0,5]),
                            [1.0,0.5,0.3333333333333333,0.25,error,0.2]).
