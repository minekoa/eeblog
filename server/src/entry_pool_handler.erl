-module(entry_pool_handler).

%% REST Callbacks
-export([ init/2
        , allowed_methods/2
        , resource_exists/2
        , content_types_provided/2 
        , content_types_accepted/2
        ]).


%% Callback Callbacks
-export([ provide_entry_list/2
        , accept_new_entry/2
        ]).


%% Model
-record( handler_options, {pool_path :: string()} ).

%%------------------------------------------------------------
%% REST Callbacks
%%------------------------------------------------------------

-spec init(cowboy_req:req(), string()) -> {'cowboy_rest', cowboy_req:req(), #handler_options{}}.
init(Req, PoolPath) ->
    { cowboy_rest
    , Req
    , #handler_options{pool_path = PoolPath }
    }.


-spec allowed_methods(cowboy_req:req(), #handler_options{})
                     -> { list(binary()), cowboy_req:req(), #handler_options{}}.
allowed_methods(Req, Opts) ->
    { [<<"GET">>, <<"POST">>], Req, Opts }.


-spec resource_exists(cowboy_req:req(), #handler_options{})
                     -> {'true'|'false', cowboy_req:req(), #handler_options{}}.
resource_exists(Req, Opts) ->
    {true, Req, Opts}.
    

-spec content_types_provided(cowboy_req:req(), #handler_options{})
                           -> { list( { {binary(), binary(), '*' | list({binary(), binary()})}
                                      , atom()
                                      } )
                              , cowboy_req:req()
                              , #handler_options{}
                              }.
content_types_provided(Req, Opts) ->    
    {[
      {{<<"application">>, <<"json">>, []}, provide_entry_list}
     ]
    , Req, Opts}.


-spec content_types_accepted(cowboy_req:req(), #handler_options{})
                            -> { list( {{binary(), binary(), list()}, atom()} )
                               , cowboy_req:req()
                               , #handler_options{}
                               }.
content_types_accepted(Req, Opts) ->
    {[
      {{<<"application">>, <<"json">>, []}, accept_new_entry}
     ], Req, Opts}.


%%------------------------------------------------------------
%% Callback Callbacks
%%------------------------------------------------------------

-spec provide_entry_list(cowboy_req:req(), #handler_options{})
                        -> {any(), cowboy_req:req(), #handler_options{}}.
provide_entry_list(Req, Opts) ->
    PoolPath = Opts#handler_options.pool_path,
    case file:list_dir(PoolPath) of
        {ok, FileNames} ->
            JsonTerm = [ {<<"entries">>,
                          [ [ {<<"id">>   , list_to_binary(F)}
%                            , {<<"title">>, unicode:characters_to_binary(get_entry_title(PoolPath, F),utf8)}
%                             , {<<"title">>, unicode:characters_to_binary(list_to_binary(get_entry_title(PoolPath, F)))}
                            , {<<"title">>, list_to_binary(entry_lib:get_entry_title(PoolPath, F))}
                            ] || F <- FileNames, is_entry_file_name(F) ] }
                       ],
            JsonString = jsx:encode(JsonTerm),
            {JsonString, Req, Opts};
        _Err ->
            {true, Req, Opts}
    end.


-spec accept_new_entry(cowboy_req:req(), #handler_options{})
                      -> {any(), cowboy_req:req(), #handler_options{}}.
accept_new_entry(Req0, Opts) ->
    PoolPath = Opts#handler_options.pool_path,
    FileName = entry_lib:create_new_entry_id(),

    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        {ok, Id, _Title, _Cats, BinData} = entry_lib:parse_entry_json(Body),
        Id = "",

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
%% Internal Use
%%------------------------------------------------------------

-spec is_entry_file_name(string()) -> boolean().
is_entry_file_name(_FileName) ->
    true.

-spec set_CORS_headers(cowboy_req:req()) -> cowboy_req:req().
set_CORS_headers(Req0) ->
    %% Cross-Origin Resouce Sharing を許可するよう レスポンスヘッダにセット
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-method">>, <<"GET, PUT, POST, DELETE, OPTIONS">>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    Req2.


