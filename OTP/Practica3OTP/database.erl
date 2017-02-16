%%%-------------------------------------------------------------------
%%% PDAP: Práctica 3 de Persistencia
%%% @author Gorka Suárez García
%%% Created : 14. ene 2016 7:34 PM
%%%-------------------------------------------------------------------

-module(database).
-author("Gorka Suárez García").
-export([observer/0, load_tuples_from_file/1, add_tuples_to_table/1,
    add_tuples_to_table_from_file/1, make_query_1/0, make_query_2/0,
    make_query_3/0]).
-include_lib("stdlib/include/ms_transform.hrl").
-define(TABLE_NAME, members_table).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

observer() ->
    observer:start().

load_tuples_from_file(Path) ->
    case open_utf8_to_read(Path) of
        fail ->
            [];
        IoDevice ->
            Lines = get_lines(IoDevice),
            file:close(IoDevice),
            Tokens = [get_tokens(X) || X <- Lines],
            Items = [convert_tokens(X) || X <- Tokens],
            [list_to_tuple(X) || X <- Items]
    end.

add_tuples_to_table(Tuples) ->
    TID = ets:new(?TABLE_NAME, [set, public, named_table]),
    ets:insert(TID, Tuples),
    TID.

add_tuples_to_table_from_file(Path) ->
    add_tuples_to_table(load_tuples_from_file(Path)).

make_query_1() ->
    %Ciudad de residencia del usuario con identi1cador 3.
    ets:match(?TABLE_NAME, {3, '_', '_', '$1'}).

make_query_2() ->
    %Nombres y apellidos de los residentes en Madrid cuyo nombre comience por C.
    ets:select(?TABLE_NAME, ets:fun2ms(
        fun ({_,[N|NS],_,"Madrid"}) when [N] =:= "C" ->
            [N|NS]
        end)).

make_query_3() ->
    %Nombres y apellidos de los usuarios que tengan entre 20 y 30 años (inclusive).
    ets:select(?TABLE_NAME, [{
        {'_', '$1', '$2', '_'},
        [{'>', '$2', 19}, {'<', '$2', 31}],
        ['$1']
    }]).

%---------------------------------------------------------------------
% Private:
%---------------------------------------------------------------------

convert_tokens(V) ->
    [to_float(to_integer(X)) || X <- V].

to_float(L) ->
    to_number(fun (X) -> list_to_float(X) end, L).
to_integer(L) ->
    to_number(fun (X) -> list_to_integer(X) end, L).

to_number(F, L) when is_list(L) ->
    try F(L) catch _:_ -> L end;
to_number(_, L) ->
    L.

get_tokens(V) ->
    V2 = string:tokens(V, ","),
    [string:strip(X, left) || X <- V2].

get_lines(IoDevice) ->
    get_lines(fun () -> io:get_line(IoDevice, "") end, []).

get_lines(Read, Lines) ->
    case Read() of
       eof ->
           lists:reverse(Lines);
       Line ->
           case lists:suffix("\n", Line) of
               true ->
                   FinalLine = lists:sublist(Line, length(Line) - 1),
                   get_lines(Read, [FinalLine | Lines]);
               _ ->
                   get_lines(Read, [Line | Lines])
           end
    end.

open_utf8_to_read(Path) ->
    try file:open(Path, [read, {encoding, utf8}]) of
        {ok, IoDevice} ->
            IoDevice;
        {error, Reason} ->
            io:format("[ERROR: open_utf8_to_read] "),
            io:format("Fail when open file, reason: ~w~n", [Reason]),
            fail
    catch
        Error:Exception ->
            io:format("[EXCEPTION: open_utf8_to_read] "),
            io:format("~w:~w~n", [Error, Exception]),
            fail
    end.
