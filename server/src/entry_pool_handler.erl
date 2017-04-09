-module(entry_pool_handler).

%% REST Callbacks
-export([ init/2
        , allowed_methods/2
        , resource_exists/2
        , content_types_provided/2 
        ]).


%% Callback Callbacks
-export([provide_entry_list/2]).

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
    { [<<"GET">>], Req, Opts }.


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
                            , {<<"title">>, list_to_binary(get_entry_title(PoolPath, F))}
                            ] || F <- FileNames, is_entry_file_name(F) ] }
                       ],
            JsonString = jsx:encode(JsonTerm),
            {JsonString, Req, Opts};
        _Err ->
            {true, Req, Opts}
    end.


%%------------------------------------------------------------
%% Internal Use
%%------------------------------------------------------------

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


-spec is_entry_file_name(string()) -> boolean().
is_entry_file_name(_FileName) ->
    true.



