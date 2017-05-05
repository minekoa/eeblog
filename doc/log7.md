# CSSを適用できるようにする

Elm で HTMLを出力する場合、
外部スタイルシートを取り込む方法はありません。

なので、

```
index.html
   ├ style.css
   └ Main.js     <- これを Main.elmから作る
```


## 1. index.htmlを作る

```html
<!DOCTYPE HTML>
<html><head><meta charset="UTF-8">
    <title>Main</title>
    <style>html,head,body { padding:0; margin:0; } body { font-family: calibri, helvetica, arial, sans-serif; }</style>
    <script type="text/javascript" src="./Main.js"></script>
    <link rel="stylesheet" type="text/css" href="style.css"></link>
</head>
<body></body>
<script type="text/javascript">
  var app =  Elm.Main.fullscreen();
</script>
</html>
```

キモは Main.js を 読み込むと、`Elm.Main` に Main.elm のモジュールがいるので、これで `fullscreen()` を呼ぶところくらい。
ちなみに、node.js のような moduleシステムがある場合 `module.exportes.Main` に展開するようになっています。

## 2. Makefile の変更

```makefile
$(SPA_OUT_DIR)/Main.js: $(SPA_SRC_DIR)/Main.elm
	$(ELM_MAKE) $(SPA_SRC_DIR)/Main.elm --output $(SPA_OUT_DIR)/Main.js
```

`--output` の出力先の拡張子を `.js` にするだけでOKです。
（もちろん `index.html` を生成しなくなったので、そこのつじつま合わせは必要です)


## 3. .js .css の静的なファイルをダウンロードできるようにする

```erlang
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
```

単純にルーティングテーブルに、 `{"/[...]", cowboy_static, {priv_dir,  eblogsv, ""}}` を加えればいいのですが、
頭に加えてしまうと、以後すべてのパスがこのテーブルに飲まれてしまうので末尾に追加しました。

本当は、`/static/[...]` のようなパスに隔離したほうがよいですね。
