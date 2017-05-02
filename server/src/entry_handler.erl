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
        FileName -> {entry_lib:exist_entry(PoolPath, FileName), Req, Opts}
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
    case entry_lib:delete_entry(PoolPath, FileName) of
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
    Title = entry_lib:get_entry_title(PoolPath, FileName),
    Cats  = [],

    case entry_lib:read_entry(PoolPath, FileName) of
        {ok, BinData} ->
            { entry_lib:make_entry_json(FileName, Title, Cats, BinData)
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
        {ok, Id, _Title, _Cats, BinData} = entry_lib:parse_entry_json(Body),
        FileName = Id,
        case entry_lib:write_entry(PoolPath, FileName, BinData) of
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



