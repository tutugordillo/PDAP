%%Autor: Pablo Gordillo
%% Ejercicio 2 Practica 1

-module(eco_client).
-export([start/0,print/1,stop/0]).

start()->
    eco_server:start().

stop()->
    ecoServer!stop.

print(Term)->
    ecoServer!{term,Term}.
