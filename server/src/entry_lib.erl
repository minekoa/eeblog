-module(entry_lib).

%% FileOperation
-export([ exist_entry/2
        , read_entry/2
        , write_entry/3
        , delete_entry/2
        , get_entry_title/2
        ]).

%% Entry Json
-export([ parse_entry_json/1
        , make_entry_json/4
        ]).

%% Entry Id
-export([ create_new_entry_id/0
        , time_to_id_string/1
        ]).


%%------------------------------------------------------------
%% FileOperation
%%------------------------------------------------------------

-spec exist_entry(string(), string()) -> boolean().
exist_entry(PoolPath, FileName) ->
    FulPath = filename:join([PoolPath, FileName]),
    filelib:is_file(FulPath).


-spec read_entry(string(), string()) -> {'ok', binary()} | {'error', atom()}.
read_entry(PoolPath, FileName) ->
    FulPath = filename:join([PoolPath, FileName]),
    file:read_file(FulPath).


-spec write_entry(string(), string(), binary()) -> {'ok', binary()} | {'error', atom()}.
write_entry(PoolPath, FileName, BinData) ->
    FulPath = filename:join([PoolPath, FileName]),
    file:write_file(FulPath, BinData).


-spec delete_entry(string(), string()) -> 'ok' | {'error', atom()}.
delete_entry(PoolPath, FileName) ->
    FulPath = filename:join([PoolPath, FileName]),
    file:delete(FulPath).


-spec get_entry_title(string(), string()) -> string().
get_entry_title(PoolPath, FileName) ->
    FullPath = filename:join([PoolPath, FileName]),
    {ok, Io} = file:open(FullPath, [read]),
    {ok, Title} = find_title(Io),
    Title.

find_title(Io) ->
    find_title( file:read_line(Io), Io ).

find_title({ok, "# " ++ Title}, _Io) ->
    S = string:strip(string:strip(Title,both,$\n)),
    {ok, S};
find_title({ok, _Line}, Io) ->
    find_title( file:read_line(Io), Io );
find_title({error,_Reason}, _Io) ->
    io:format("error:~p~n",[_Reason]),
    {error, ""};
find_title(eof, _Io) ->
    {ok, ""}.


%%------------------------------------------------------------
%% Entry Json
%%------------------------------------------------------------

-spec parse_entry_json(binary()) -> {'ok', string(), string(), list(string()), binary()} | {'error', atom()}.
parse_entry_json(JsonText) ->
    JsonTerm = jsx:decode(JsonText),

    case lists:keysort(1, JsonTerm) of
        [ {<<"content">>, Content}, {<<"id">>, Id}, {<<"title">>, Title}] ->
            {ok, binary_to_list(Id), binary_to_list(Title), [], Content};
        _ ->
            {error, badjson}
    end.


-spec make_entry_json(string(), string(), list(string()), binary()) -> binary().
make_entry_json(Id, Title, _Cats, BinData) ->
    JsonTerm = [ {<<"id">>, list_to_binary(Id)}
               , {<<"title">>, list_to_binary(Title)}
               , {<<"content">>, BinData}
               ],
    jsx:encode(JsonTerm).


%%------------------------------------------------------------
%% Entry Id
%%------------------------------------------------------------

-spec create_new_entry_id() -> string().
create_new_entry_id() ->
    LocalTime = calendar:local_time(),
    time_to_id_string( LocalTime ).
    
-spec time_to_id_string( {{integer(), integer(), integer()}, {integer(), integer(), integer()}} ) -> string().
time_to_id_string( {{Year, Month, Day}, {Hour, Min, Sec}} )->
    %            YYYY   MM       DD  -   hh     mm     ss     
    io_lib:format("~p~2.10.0B~2.10.0B-~2.10.0B~2.10.0B~2.10.0B",
                  [Year, Month, Day, Hour, Min, Sec]).
