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
```
$ git clone https://github.com/rebar/rebar3.git
$ cd rebar3
$ ./bootstrup
```
3. 環境変数 `PATH` に `C:\Users\xxx\rebar3` を追加します
4. (Git Bash を再起動します)

(参考 (Windowsでrebar3を使う -Qiita)[http://qiita.com/leafia78/items/240ea9d06a8e50683787])

## プロジェクトの作成

$ rebar3 new app ecowtest

```
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

```
{erl_opts, [debug_info]}.
{deps, [
  {cowboy, ".*", {git, "git://github.com/ninenines/cowboy" , {tag, "2.0.0-pre.4"}}}
]}.
```

```
{release, {ecowtest, "1"}, [ecowtest]}.
{extended_start_script, true}.
```

```
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

$ rebar3 compile

$ rebar3 release
$ _build/default/rel/ecowtest/bin/ecowtest.cmd console

