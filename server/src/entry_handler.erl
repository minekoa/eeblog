-module(entry_handler).

%% REST Callbacks
-export([ init/2
        , allowed_methods/2
        , resource_exists/2
        , content_types_provided/2
        , content_types_accepted/2
        , delete_resource/2
        ]).

%% Callback Callbacks

-export([ provide_entry/2
        , accept_entry/2
        ]).

%% Model
-record( handler_options, {pool_path :: string()} ).


%%------------------------------------------------------------
%% REST Callbacks
%%------------------------------------------------------------

-spec init(cowboy_req:req(), string()) -> { 'cowboy_rest', cowboy_req:req(), #handler_options{} }.
init(Req, PoolPath) ->
    { cowboy_rest
    , Req
    , #handler_options{pool_path = PoolPath}
    }.


-spec allowed_methods(cowboy_req:req(), #handler_options{})
                     -> { list(binary()), cowboy_req:req(), #handler_options{} }.
allowed_methods(Req, Opts) ->
    { [<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, Opts }.


-spec resource_exists(cowboy_req:rew(), #handler_options{}) 
                     -> {boolean(), cowboy_req:req(), #handler_options{}}.
resource_exists(Req, Opts) ->      
    PoolPath = Opts#handler_options.pool_path,
    case filename(Req) of
        [] -> {false, Req, Opts};
        FileName -> {exist_entry(PoolPath, FileName), Req, Opts}
    end.


-spec content_types_provided(cowboy_req:req(), #handler_options{})
                            -> { list( {{binary(), binary(), list()}, atom()} )
                               , cowboy_req:req()
                               , #handler_options{}
                               }.
content_types_provided(Req, Opts) ->
    {[
      {{<<"application">>, <<"json">>, []}, provide_entry}
     ], Req, Opts}.


-spec content_types_accepted(cowboy_req:req(), #handler_options{})
                            -> { list( {{binary(), binary(), list()}, atom()} )
                               , cowboy_req:req()
                               , #handler_options{}
                               }.
content_types_accepted(Req, Opts) ->
    {[
      {{<<"application">>, <<"json">>, []}, accept_entry}
     ], Req, Opts}.


-spec delete_resource(cowboy_req:req(), #handler_options{})
                     -> {boolean(), cowboy_req:req(), #handler_options{}}.
delete_resource(Req0, Opts) ->
    PoolPath = Opts#handler_options.pool_path,
    FileName = filename(Req0),
    case delete_entry(PoolPath, FileName) of
        ok -> {true, Req0, Opts};
        _Err -> {false, Req0, Opts}
    end.


%%------------------------------------------------------------
%% Callback Callbacks
%%------------------------------------------------------------

-spec provide_entry(cowboy_req:req(), #handler_options{}) -> {any(), cowboy_req:req(), #handler_options{}}.
provide_entry(Req0, Opts) ->
    PoolPath = Opts#handler_options.pool_path,
    FileName = filename(Req0),
    Title = get_entry_title(PoolPath, FileName),
    Cats  = [],

    case read_entry(PoolPath, FileName) of
        {ok, BinData} ->
            { make_entry_json(FileName, Title, Cats, BinData)
            , set_CORS_headers(Req0)
            , Opts
            };
        _Err -> % 404 Not Found
            Req1 = cowboy_req:reply(404, Req0),
            {halt, Req1, Opts} % node: Cowboy 1.0 ではドキュメントにあったけど、2.0ではたまたま動いている
    end.


-spec accept_entry(cowboy_req:req(), #handler_options{}) -> {any(), cowboy_req:req(), #handler_options{}}.
accept_entry(Req0, Opts) ->
    PoolPath = Opts#handler_options.pool_path,
    FileName = filename(Req0),

    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        {ok, Id, _Title, _Cats, BinData} = parse_entry_json(Body),
        FileName = Id,
        case write_entry(PoolPath, FileName, BinData) of
            ok ->
                {true, set_CORS_headers(Req1), Opts};
            _Err -> % 409 Conflict
                Req2 = cowboy_req:reply( 409, Req1 ),
                {halt, Req2, Opts}
        end
    of
        {Bool, Req_r, Opts} -> {Bool, Req_r, Opts}
    catch
        error:_Reason -> % 400 Bad Request
            {false, Req0, Opts}
    end.

%%------------------------------------------------------------
%% Utility
%%------------------------------------------------------------

-spec filename(cowboy_req:req()) -> string().
filename(Req) ->
    case cowboy_req:path_info(Req) of
        [Path] -> binary_to_list(Path);
        _ -> []
    end.

-spec set_CORS_headers(cowboy_req:req()) -> cowboy_req:req().
set_CORS_headers(Req0) ->
    %% Cross-Origin Resouce Sharing を許可するよう レスポンスヘッダにセット
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-method">>, <<"GET, PUT, POST, DELETE, OPTIONS">>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    Req2.

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
%% Json
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


