# WebServer で Hello

## Cowboy の準備

Cowboy は、Erlang の事実上の標準Webサーバーです。これを使うための設定を行います。

### reber.config

まずはパッケージを取得するために rebar にその設定を追加します。
rebar.config は最初に生成されたままなので以下のようになっているはずです。

```erlang:eblogsv/rebar.config
{erl_opts, [debug_info]}.
{deps, []}.
```

ここに、cowboy を入れます

```erlang:eblogsv/rebar.config(修正後)
{erl_opts, [debug_info]}.
{deps, [
  {cowboy, ".*", {git, "git://github.com/ninenines/cowboy" , {tag, "2.0.0-pre.4"}}}
]}.
```
これで初回ビルド時に cowboy とその依存ライブラリを取得してきてくれます。

### src/eblogsv.app.src

cowboy は OTPアプリケーションのなので、app.src に追加する必要があります。
（でないと、実行時に cowboy がいないことになります）

こちらも最初に生成されたままなので、以下のようになっているはず。

```erlang:eblogsv/src/eblogsv.app.src
{application, eblogsv,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { eblogsv_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
```

ここにもcowboy を追加します（ついでに description も変えてしまいましょう）。

```erlang:eblogsv/src/eblogsv.app.src(修正後)
{application, eblogsv,
 [{description, "EBlog Server"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { eblogsv_app, []}},
  {applications,
   [kernel,
    stdlib,
    cowboy
   ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
```

ちなみに dialyzer もここを見ているような気がします。

最初、ここのapplicationsにcowboyを入れ忘れていた時、
2.0pre3 => 2.0pre4 で変わってしまった関数
(cowboy:start_http/4 がなくなり start_clear/4 になった）を見つけてくれなかった。


## WebでHello

では、HelloWorldするために、コードを書いていきましょう。

eblogsv/src/eblogsv_app.erl の start/2 関数を以下のように修正します。

```erlang:eblogsv/src/eblogsv_app.erl
start(_StartType, _StartArgs) ->
    Route = [ {
        '_',
        [
           {"/",cowboy_static, {priv_file, eblogsv,"index.html"}}
        ]
    } ],
    Dispatch = cowboy_router:compile(Route),
    NumAcceptors = 100,
    TransOpts    = [{port, 8080}],
    ProtocolOpts  = #{env => #{dispatch => Dispatch}},

    {ok, _} = cowboy:start_clear(eblog, NumAcceptors, TransOpts, ProtocolOpts),

    eblogsv_sup:start_link().
```

細かい説明は追々。簡単に言えば、cowboy_router:compile/1 でRouteを元にルーティングテーブルを作って、
あとは各種オプションを揃えて [cowboy:start_clear/4](https://ninenines.eu/docs/en/cowboy/2.0/manual/cowboy.start_clear/) で HTTPサーバーをスタートする、という記述です。

URL "/"  に割り当てたハンドラ [cowboy_static](https://ninenines.eu/docs/en/cowboy/2.0/manual/cowboy_static/) は静的ファイルをルーティングします。
Erlang の慣習なのかな？、priv というディレクトリに静的リソースを収めるのがよくあるスタイルのようで、
`{priv_file, eblogsv, "index.html"}` は、priv にあるファイル、アプリケーション名(atom)、パス を指定しています。


次に、実際に表示する htmlファイルを用意します。eblog/eblogsv/priv を `mkdir` してそこに index.html を作成します

```html:eblogsv/priv/index.html
<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>eblog(仮)</title>
</head>
<body>
  <h1>eblog(仮)</h1>
  <p>こんにちは、こんにちは！</p>
</body>
</html>
```

あとは make run すれば、http://localhost:8080/ で こんにちは してくれるはずです。

