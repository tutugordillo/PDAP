-module(quicksort).
-export([sort/1]).

    
sort([]) -> [];
sort([X|L]) ->
    Left = [Y || Y <- L, Y < X],
    Right = [Y || Y <- L, Y >= X],
    Recv = self(),
    Pid = spawn(fun() ->   Result = sort(Right),
   		     	   Recv ! {self(), Result}
	  end),
    ResultL = sort(Left),
    receive
       {Pid,ResultR} -> 
           ResultL ++ [X] ++ ResultR
    end.


