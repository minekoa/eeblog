%%%-------------------------------------------------------------------
%% @doc eblogsv public API
%% @end
%%%-------------------------------------------------------------------

-module(eblogsv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Route = [ {
        '_',
        [
         {"/"                 , cowboy_static     , {priv_file, eblogsv,"index.html"}},
         {"/v1/entries"       , entry_pool_handler, os:getenv("HOME") ++ "/work/reps/eeblog/tmp/contents"},
         {"/v1/entries/[...]" , entry_handler     , os:getenv("HOME") ++ "/work/reps/eeblog/tmp/contents"},
         {"/[...]"            , cowboy_static     , {priv_dir,  eblogsv, ""}}
        ]
    } ],
    Dispatch = cowboy_router:compile(Route),
    NumAcceptors = 100,
    TransOpts    = [{port, 8080}],
    ProtocolOpts  = #{env => #{dispatch => Dispatch}},

    {ok, _} = cowboy:start_clear(eblog, NumAcceptors, TransOpts, ProtocolOpts),

    eblogsv_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
