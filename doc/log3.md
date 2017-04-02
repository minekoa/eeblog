# cowboy_rest を使う

それでは、さっそくRESTサーバーを実装していきましょう。

cowboy には、RESTサーバーを実装するためのモジュール `cowboy_rest` があります。

* [Nine Nines: cowboy_rest(3) ](https://ninenines.eu/docs/en/cowboy/2.0/manual/cowboy_rest/)

`cowboy_rest` モジュールは、REST API ハンドラーを作るためのステートマシーンのフレームワーク提供していて、適切なコールバック関数を実装することで、
ほぼほぼ良い感じのREST処理を行ってくれます。

REST API ハンドラーを実装する上で、`cowboy_rest` モジュールを使うか否かの判断は、返したい HTTPステータスコード が、
このモジュールのルールに則っているかで決めるとよいでしょう。
`cowboy_rest` モジュールは、持ち回される `Req` 引数 (`cowboy_req:req()`型)を編集することで、レスポンスに関してはユーザの好き勝手につくれるフレームワークになっていますが、
唯一、HTTPステータスコードだけはユーザーに好き勝手させない作りになっています。

もちろん、`cowboy_rest`まかせでも「問題のない」ステータスコードを返してくれますが、ステータスコードの振る舞いが仕様で定められている場合、
このフレームワークを使わないほうが楽に見通しの良いコードを作ることができます。

まずは、[Nine Nines: REST flowcharts](https://ninenines.eu/docs/en/cowboy/2.0/guide/rest_flowcharts/) を確認して
ほしい振る舞いが手に入るか検討してみてください。


## ブログ記事の一覧を返すハンドラー

さてさて、面倒くさい話はここまでにして、まずは第一歩。ブログの記事一覧を返すAPIを作りましょう。
`entry_pool_handler` モジュールを作ります。

今回定義するコールバックは以下の４つ。

|callback                | 説明 |
|------------------------|------|
|`init/2`                |      |
|`allowed_methods/2`     | サポートする HTTPメソッドのリストを定義します。ここにないメソッドの場合、`405 method not allowd` になります|
|`resouce_exists/2`      | リソースの有無を返します。Falseを返す場合、`404 Not Found` になります|
|`content_types_provided`| `GET` `HEAD` リクエスト時にレスポンスをつくる関数を `content_type` ごとにマップします。マッチしない場合、`406 not acceptable` になります|


note
: cowboy_rest は コールバック関数の実装が必須ではなないので、behavior になっていません。

### init/2

```
-spec init(cowboy_req:req(), string()) -> {'cowboy_rest', cowboy_req:req(), #handler_options{}}.
init(Req, PoolPath) ->
    { cowboy_rest
    , Req
    , #handler_options{pool_path = PoolPath }
    }.
```

返り値のタプルの最初の要素を`cowboy_rest` にすると、以後 `cowboy_rest` モジュールが使われます。


### allowed_methods/2

```
-spec allowed_methods(cowboy_req:req(), #handler_options{})
                     -> { list(binary()), cowboy_req:req(), #handler_options{}}.
allowed_methods(Req, Opts) ->
    { [<<"GET">>], Req, Opts }.
```

今のところ GETメソッドしかサポートしません

### resource_exists/2

```
-spec resource_exists(cowboy_req:req(), #handler_options{})
                     -> {'true'|'false', cowboy_req:req(), #handler_options{}}.
resource_exists(Req, Opts) ->
    {true, Req, Opts}.
```

いまのところ、必ずあるということになりますので、`true` です。
本当は、エントリーを格納するフォルダーがあるか？を返そうと考えていましたが、
500 internal server error じゃないかなぁという気がします。

### content_types_provided/2

```
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
```

このように書くことで、レスポンスボディを作る際に `provide_entry_list` をコールバックしてくれます。
（なの忘れずに`export`しておいてください)

ちなみに `provide_entry_list` はこんな感じにしました。

```
-spec provide_entry_list(cowboy_req:req(), #handler_options{})
                        -> {any(), cowboy_req:req(), #handler_options{}}.
provide_entry_list(Req, Opts) ->
    PoolPath = Opts#handler_options.pool_path,
    case file:list_dir(PoolPath) of
        {ok, FileNames} ->
            JsonTerm = [ {<<"entries">>,
                          [ [ {<<"id">>   , list_to_binary(F)}
                            , {<<"title">>, list_to_binary(get_entry_title(PoolPath, F))}
                            ] || F <- FileNames, is_entry_file_name(F) ] }
                       ],
            JsonString = jsx:encode(JsonTerm),
            {JsonString, Req, Opts};
        _Err ->
            {true, Req, Opts}
    end.
```

Json を作るため、jsx モジュールを使っています。

## その他の準備

`eblogsv_app.erl` に URLとハンドラーのマッピングを記述します。

```
start(_StartType, _StartArgs) ->
    Route = [ {
        '_',
        [
         {"/"          , cowboy_static     , {priv_file, eblogsv,"index.html"}},
         {"/v1/entries", entry_pool_handler, os:getenv("HOME") ++ "/work/reps/eblog/tmp/contents"}
        ]
    } ],
    Dispatch = cowboy_router:compile(Route),
    NumAcceptors = 100,
    TransOpts    = [{port, 8080}],
    ProtocolOpts  = #{env => #{dispatch => Dispatch}},

    {ok, _} = cowboy:start_clear(eblog, NumAcceptors, TransOpts, ProtocolOpts),

    eblogsv_sup:start_link().
```

コンテンツ置き場の実パスはとりあえず `$HOME/work/reps/eblog/tmp/contents` とかにしてみます。
(あとでちゃんとしたものを考えましょう)


その他の準備のとして、
jsx を使うようになったので、`eblogsv.app.src`と`rebar.config`にこれを追加します。


`eblogsv.app.src`

```
{application, eblogsv,
 [{description, "EBlog Server"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { eblogsv_app, []}},
  {applications,
   [kernel,
    stdlib,
    cowboy,
    jsx
   ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
```

`rebar.config`

```
{erl_opts, [debug_info]}.
{deps, [
  {jsx,    ".*", {git, "git://github.com/talentdeficit/jsx",   {tag, "v2.8.2"}}},
  {cowboy, ".*", {git, "git://github.com/ninenines/cowboy" , {tag, "2.0.0-pre.4"}}}
]}.
```
