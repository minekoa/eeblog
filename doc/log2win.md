# Windows での環境構築

* (Windows10 64bit環境でやってみた)
* (cowboyが動くところまでを確認してみた)

## Erlang のインストール

1. http://www.erlang.org/downloads から `OTP 20.0 Windows 64-bit Binary File` をダウンロードします
2. 実行します
3. 環境変数 `PATH` に `C:\Program Files\erl9.0\bin` を追加します

## Git Bash をインストールする

（ずっと前に入れたものなので、思い出せない。割愛）

## rebar3 のインストール

1. Git Bash を起動します
2. 以下を実行します
```console
$ git clone https://github.com/rebar/rebar3.git
$ cd rebar3
$ ./bootstrup
```
3. 環境変数 `PATH` に `C:\Users\xxx\rebar3` を追加します
4. (Git Bash を再起動します)

(参考 (Windowsでrebar3を使う -Qiita)[http://qiita.com/leafia78/items/240ea9d06a8e50683787])

## プロジェクトの作成

1. Git Bash を起動します
2. `$ rebar3 new app ecowtest` を実行します（`ecowtest`は作成するプロジェクト名です。「ErlangとCOWboyが動くかどうかTESTする」の意です)
3. 以下の内容で `ecowtest/relx.config` ファイルを作成します（リリースビルドに必要です）
```erlang
{release, {ecowtest, "1"}, [ecowtest]}.
{extended_start_script, true}.
```

## Cowboy (Webアプリケーションサーバー)の準備

1. `ecowtest/reber.config` の `deps`に `cowboy` の情報を追加します。バージョンは2.0.0-pre4 を使います。修正後は以下のようになります。
```erlang
{erl_opts, [debug_info]}.
{deps, [
  {cowboy, ".*", {git, "git://github.com/ninenines/cowboy" , {tag, "2.0.0-pre.4"}}}
]}.
```
2. `ecowtest/src/ecowtest.app.src`の `applications` に `cowboy` を追加します。修正後は以下のようになります。
```erlang
{application, ecowtest,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { ecowtest_app, []}},
  {applications,
   [kernel,
    stdlib,
    cowboy
   ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
```

## Webアプリケーションの作成

1. `ecowtest/src/ecowtest_app.erl` の `start/2` 関数を以下のように修正します。
```erlang
start(_StartType, _StartArgs) ->
    Route = [ {
        '_',
        [
            {"/", cowboy_static, {priv_file, ecowtest, "index.html"}}
        ]
    }],
    Dispatch = cowboy_router:compile(Route),
    NumAcceptors = 100,
    TransOpts = [{port, 8080}],
    ProtocolOpts = #{env => #{dispatch => Dispatch}},

    {ok, _} = cowboy:start_clear(ecowtest, NumAcceptors, TransOpts, ProtocolOpts),

    ecowtest_sup:start_link().
```
2. `ecowtest/priv/index.html` を以下の内容で作ります
```html
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
</html>```

## ビルドと実行

1. 以下のコマンドを実行します。werl が起動してその中にErlang Shell が表示されます
```console
$ rebar3 compile
$ rebar3 release
$ _build/default/rel/ecowtest/bin/ecowtest.cmd console
```
2. ブラウザーで `http://localhost:8080/` にアクセスします。先ほど作った `index.html` が表示されたら成功です。


