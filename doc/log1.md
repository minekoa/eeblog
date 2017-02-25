# Erlang と Elm で BLOG SPA を作る

## ブロジェクトを作る(Server Side)

```
$ mkdir eblog
$ cd eblog
$ rebar3 new app eblogsv
===> Writing eblogsv/src/eblogsv_app.erl
===> Writing eblogsv/src/eblogsv_sup.erl
===> Writing eblogsv/src/eblogsv.app.src
===> Writing eblogsv/rebar.config
===> Writing eblogsv/.gitignore
===> Writing eblogsv/LICENSE
===> Writing eblogsv/README.md
```

結果、こんなのができるはず

```
$ tree
.
└── eblogsv
    ├── LICENSE
    ├── README.md
    ├── rebar.config
    └── src
        ├── eblogsv.app.src
        ├── eblogsv_app.erl
        └── eblogsv_sup.erl
```

細かい説明はおいおいとして、ざっくりとした説明をするとこんなかんじ

|ファイル名         |説明                            |
|-------------------|--------------------------------|
|rebar.config       |rebarの設定                     |
|src/eblogsv.app.src|アプリケーションのいろいろな設定|
|src/eblogsv_app.erl|アプリケーションのソースコード  |
|src/eblogsv_sup.erl|スーパーバイザー                |

この段階で一度コンパイルしてみましょう。

```
$ cd eblogsv/
$ rebar3 compile
===> Verifying dependencies...
===> Compiling eblogsv
```

_build フォルダ以下にビルド成果物が作られます。

```
$ tree
.
├── LICENSE
├── README.md
├── _build
│   └── default
│       └── lib
│           └── eblogsv
│               ├── ebin
│               │   ├── eblogsv.app
│               │   ├── eblogsv_app.beam
│               │   └── eblogsv_sup.beam
│               ├── include -> ../../../../include
│               ├── priv -> ../../../../priv
│               └── src -> ../../../../src
├── rebar.config
├── rebar.lock
└── src
    ├── eblogsv.app.src
    ├── eblogsv_app.erl
    └── eblogsv_sup.erl
```

## まずはコンソールにハローワールド

eblogsv_app.erl をひらくとこんな自動生成コードがおさまっているはずです。

```erlang:eblogsv/src/eblogsv_app.erl
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
    eblogsv_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
```

これを少し変えて、コンソールにハローするようにします。start/2 関数に

```erlang
start(_StartType _StartArgs) ->
    io:format("Hello World!~n"),
    eblogsv_sup:start_link().
```

のように付け加えます。

先ほどのように rebar3 compile してもよいのですが、せっかくなのでMakefileを整えましょう。
eblog/Makefile はこんな感じです。

```makefile:Makefile
REBAR3=rebar3
PROGRAM_SV=eblogsv
SV_DIR=eblogsv
RELPATH=./$(SV_DIR)/_build/default/rel/$(PROGRAM)

.PHONY: all compile release clean


all: release

compile:
	cd $(SV_DIR);$(REBAR3) compile
	cd $(SV_DIR);$(REBAR3) dialyzer

release: compile
	cd $(SV_DIR);$(REBAR3) release

clean:
	cd $(SV_DIR);$(REBAR3) clean

run: release
	$(RELPATH)/bin/$(PROGRAM_SV) console
```

将来 elm 側のソースとビルド環境を別ディレクトリに並べたいので、eblog/eblogsv と階層化させてますが、
そのおかげでちょっと面倒な記述になっている以外は至って普通の Makefile です。

あと、relx (リリースツール）用の設定ファイルを eblogsv/relx.config に置きます

```erlang:eblogsv/relx.config
{release, {eblogsv, "1"}, [eblogsv]}.
{extended_start_script, true}.
```

これで、`make run` を実行すれば、いかのような表示が見れるはずです。

```
$ make run
cd eblogsv;rebar3 compile
  ・
  ・
cd eblogsv;rebar3 dialyzer
  ・
  ・
cd eblogsv;rebar3 release
  ・
  ・
./eblogsv/_build/default/rel/eblogsv/bin/eblogsv console
  ・
  ・
Erlang/OTP 18 [erts-7.2.1] [source] [smp:4:4] [async-threads:10] [kernel-poll:false]

Hello World!
Eshell V7.2.1  (abort with ^G)
(eblogsv@localhost)1>
```

プログラムを終了するには、`Ctrl-C` のあと `a` を入力してください。
