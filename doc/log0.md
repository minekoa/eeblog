# 環境構築

## erlang

まずはビルドに必要な一通りをインストール。

```
$ sudo apt install build-essential libncurses5-dev libssl-dev curl
```

ソースコードを取得して展開。

```
$ curl -LO http://erlang.org/download/otp_src_19.3.tar.gz
$ tar -zxvf otp_src_19.3.tar.gz
$ cd otp_src_19.3
```

`.configure` します。

```
$ ./configure --enable-hipe --enable-dtrace --without-javac
```

* `jinterface` 
* `odbc` 
* `wx` 
* `documentation` 

が 使えません。前者２つは`--without-javac`したから、後者はビルドに必要な環境がたりていないからです。

今回はそれでよしとして、`make` `make install` します。

```
$ make
$ sudo make install 
```

## rebar3 (erlang build system)

```
$ git clone https://github.com/erlang/rebar3.git
$ cd rebar3/
$ ./bootstrap 
$ ./rebar3 local install
```

あとは、`$HOME/.cache/rebar3/bin` にパスを通します。


## elm

`npm` をつかってインストールします。

```
$ npm install -g elm
```

インストールできたか確認。

```
$ elm -v
0.18.0
```

