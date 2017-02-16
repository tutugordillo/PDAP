-module(area_server).

-export([init/1, start/0, terminate/2, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3]).

-behaviour(gen_server).

start() -> gen_server:start_link(?MODULE, [], []).

handle_call({rectangle, Width, Height}, _From, State) ->
  {reply, Width * Height, State};
handle_call({circle, Radius}, _From, State) -> 
  {reply, math:pi() * Radius * Radius, State};
handle_call({hack, Pid}, _From, State) -> 
  gen_server:reply(Pid, respuesta),
  {reply, none, State};
handle_call(Other, _From, State) ->
  {reply, {error, Other}, State}.

init(_) ->  
    io:format("AREA Servidor inicializado!~n"),
    process_flag(trap_exit, true),
    {ok, ok}.

handle_cast(Request, State) ->
  io:format("Unexpected request: ~w~n", [Request]),
  {noreply, State}.

handle_info(Message, State) ->
  io:format("Unexpected message: ~w~n", [Message]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("Area server finished.~n"),
  io:format("Reason: ~w~n", [Reason]).

code_change(PreviousVersion, State, _Extra) ->
  io:format("Code change from ~w~n", [PreviousVersion]),
  {ok, State}.
