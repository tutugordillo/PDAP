-module(afile_server).

-export([start/1, init/1, terminate/2, handle_call/3, handle_cast/2,
      handle_info/2, code_change/3]).

-behaviour(gen_server).

start(Dir) -> gen_server:start_link(?MODULE, Dir, []).

init(Dir) -> 
    io:format("AFILE Servidor inicializado!~n"),
    process_flag(trap_exit, true),
    {ok, Dir}.

handle_call(list_dir, _From, Dir) -> 
    {reply, file:list_dir(Dir), Dir};
handle_call({get_file, File}, _From, Dir) -> 
    Full = filename:join(Dir, File),
    {reply, file:read_file(Full), Dir}.

handle_cast({change_dir, NewDir}, _Dir) ->
    {noreply, NewDir}.

handle_info(Message, State) ->
    io:format("Unexpected message: ~w~n", [Message]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("File server finished.~n"),
    io:format("Reason: ~w~n", [Reason]).

code_change(PreviousVersion, State, _Extra) ->
    io:format("Code change from ~w~n", [PreviousVersion]),
    {ok, State}.  
