-module(ticket_server).

-export([init/1, start/1, terminate/2, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, get_ticket/0]).

-behaviour(gen_server).
-define(SERVERNAME, ?MODULE).

start(N) -> gen_server:start_link(?MODULE, N, []).

get_ticket() -> gen_server:call(?SERVERNAME, get_ticket).

init(N) when N > 0 -> 
    io:format("TICKET Servidor inicializado!~n"),
    process_flag(trap_exit, true),
    {ok, lists:seq(1,N)};

init(N) -> {stop, {bad_parameter, N}}.

handle_call(get_ticket, _From, []) -> {stop, no_tickets, error, []};
handle_call(get_ticket, _From, [T]) -> {stop, normal, T, []};
handle_call(get_ticket, _From, [T|Ts]) -> {reply, T, Ts}.

handle_cast(Request, State) ->
  io:format("Unexpected request: ~w~n", [Request]),
  {noreply, State}.

handle_info(Message, State) ->
  io:format("Unexpected message: ~w~n", [Message]),
  {noreply, State}.
  
terminate(Reason, _State) ->
  io:format("Ticket server finished.~n"),
  io:format("Reason: ~w~n", [Reason]).


code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.
  
