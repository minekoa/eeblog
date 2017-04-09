# REST Client を Elm でつくる

サーバー側の最低限の準備が終了しましたので、クライアントを作りましょう。
（プログラミングのモチベーション維持のためには、まずは動くものを手っ取り早く作ってしまうのが重要です。)


## ディレクトリ構成

`server/priv/index.html` を Elmで作ります。そのためのElmコードは
`spa` 下に配置します。

こんな感じです。

```
.
├── Makefile
├── README.md
├── elm-package.json
├── server
│   ├── LICENSE
│   ├── README.md
│   ├── priv
│   │   └── (index.html)    ... Elm で生成される SPA になります
│   ├── rebar.config
│   ├── relx.config
│   └── src
│       ├── eblogsv.app.src
│       ├── eblogsv_app.erl
│       ├── eblogsv_sup.erl
│       └── entry_pool_handler.erl
└── spa
　   └── Main.elm             ... 新しく追加
```

## ビルド環境

### Makefile

まずは Elm の依存パッケージをインストールする

```
env:
	elm-package install 
```

と、`server/priv/index.html` を Elm コードからビルドするルール

```
server/priv/index.html: spa/Main.elm
	elm-make spa/Main.elm --output server/priv/index.html
```

Elm Applicationはビルドするとひとつのhtmlファイル またはひとつの JavaScriptファイルが生成されますので、
`%.js: %.elm` のような makeの型ルールを書かなくてもOKです。

これを、サーバーの `compile` ターゲットの依存ターゲットにしてあげます。

追加し終えたものはこんな感じ。


```makefile
REBAR3=rebar3
ELM_MAKE=elm-make
ELM_PACKAGE=elm-package

PROGRAM_SV=eblogsv
SV_DIR=server
SPA_SRC_DIR=spa
SPA_OUT_DIR=$(SV_DIR)/priv
RELPATH=./$(SV_DIR)/_build/default/rel/$(PROGRAM_SV)

.PHONY: all env compile release clean


all: release

env:
	$(ELM_PACKAGE) install

$(SPA_OUT_DIR)/index.html: $(SPA_SRC_DIR)/Main.elm
	$(ELM_MAKE) $(SPA_SRC_DIR)/Main.elm --output $(SPA_OUT_DIR)/index.html

compile: $(SPA_OUT_DIR)/index.html
	cd $(SV_DIR);$(REBAR3) compile
	cd $(SV_DIR);$(REBAR3) dialyzer

release: compile
	cd $(SV_DIR);$(REBAR3) release

clean:
	cd $(SV_DIR);$(REBAR3) clean
	rm $(SPA_OUT_DIR)/index.html

run: release
	$(RELPATH)/bin/$(PROGRAM_SV) console
```

### elm-package.json

`elm-make` を一度すれば、elm-package.json というファイルが作られます。
これをベースにして、

今回、elm-lang/http パッケージを使用してますので、`dependencies` に それに対するルールを追加します。
もうひとつ、今回 elm-make は Main.elm のあるフォルダの一階層上から叩きますので、
`souce-directories` は、`"."` から `"spa"` に書き換えます。

できたのがこれです。

```json:elm-package.json
{
    "version": "1.0.0",
    "summary": "helpful summary of your project, less than 80 characters",
    "repository": "https://github.com/user/project.git",
    "license": "BSD3",
    "source-directories": [
        "spa"
    ],
    "exposed-modules": [],
    "dependencies": {
        "elm-lang/core": "5.1.1 <= v < 6.0.0",
        "elm-lang/html": "2.0.0 <= v < 3.0.0",
        "elm-lang/http": "1.0.0 <= v < 3.0.1"
    },
    "elm-version": "0.18.0 <= v < 0.19.0"
}
```

## 

今回、Elm のアプリケーション種別としては、`Html.program` を使います。
ElmでSPAを作りたいときは、基本的にはこれをえらべばOKです。

`Html.program` は、`init`, `view`, `update`, `subscriptions` の4つの関数を与えてあげます。

The Elm Architecture というフレームワークに則って記述します。
このフレームワークは、アプリケーションを状態遷移モデルとして記述します。

基本的には、Model (アプリケーションの持つ状態の型) と Msg (この状態遷移モデルが受け取るイベントの集合の型) という2つの型を用意して、

* 'update' ..イベントを受け取ったら Modelを更新する
* 'view'   ..モデルをHTMLにレンダリングする
* 


### Model

まずは SPAの大きなビューの状態である `Page` を直和型で、エントリーの情報を `EntryInfo` というレコードで表現します。

```elm
type Page
    = TopPage
    | EntryListPage

type alias EntryInfo =
    { id : String
    , title : String
    }
```

Elm の文法は Haskell によく似ているので、見れば解ると思いますが、
型注釈が Haskellの`::` から `:` に、Haskellの`type` （型シノニム）は `type alias` に、`data` （コンストラクタ）は `type` に置き換わっています。
（Haskellがそういうキーワードをとった理由はわかるのですが、わたしはElmのキーワード・チョイスのほうが好きです）

そして、Pageとエントリーの情報のリスト、もひとつ、メッセージ表示を 状態としてもつ `Model` を定義します。

```elm
type alias Model =
    { page : Page
    , entry_list : List EntryInfo
    , message : Maybe String
    }
```

### Msg と update

The Elm Architecture を状態遷移モデルと捉えるとき、`Msg` はイベントの集合、`update` はイベントに対する事後状態のマップです。
非常に見通しの良いコードになるので、わたしが Elm が大好きです。

`Msg` は直和型で定義します。

```elm
type Msg
    = RequestEntryList
    | ShowEntryList (Result Http.Error (List EntryInfo))
```

メッセージにはペイロードをつけることができます。RequestEntryList は置いといて、
`ShowEntryList (Result Http.Error (List EntryInfo))` を少し説明しましょう。

[Result](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) は、以下のような型です

> ```elm
> type Result error value
>     = Ok value
>     | Err error
> ```

EntryListはAjax (Elm の Httpパッケージ）を利用して取得するので、失敗時の型引数 `error` に Http.Errorを、 
`value` に 取得成功したとき得られる値の型 List EntryInfo を与えているというわけです。

これを踏まえて update を見てみましょう。

```elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RequestEntryList ->
            ( model
            , requestEntryList )

        ShowEntryList (Ok entry_list) ->
            ( { model | page = EntryListPage
                      , entry_list = entry_list }
            , Cmd.none )

        ShowEntryList (Err _) ->
            ( { model | message = Just("エントリー一覧を取得できませんでした") }
            , Cmd.none)
```

update は Msg と Model を受け取って、(Model, Cmd Msg) のペアを返します。
単に Modelを返さないところがミソで、事後に実行してほしい コマンドを返すことができます。

こうすることであるイベントをトリガーに
内部で処理を実行し、その結果をもって初めて Modelを更新する、ということが可能になります。

今回は、RequestEntryList イベントをトリガーに Ajax で Entry のリストを取りに行き、
取得できたら Model に反映する.. という流れですので、まさにこの仕組みがうってつけです。

また、case にはパターンを書くことができるので、失敗時の遷移と成功時の遷移をこうして
場合分けするのが良い感じでしょう。

(requestEntryList というコマンドについては、後述しますので、とりあえず置いときましょう)

### view

view については特に説明することもないでしょう。とりあえずこんなコードです。

```elm
view : Model -> Html Msg
view model = div []
             [ h1 [] [text "EE Blog (Erlang×Elm)"]
             , navibar model.page
             , messageLine model.message
             , case model.page of
                   TopPage ->       topPage model
                   EntryListPage -> entryListPage model.entry_list
             ]

navibar : Page -> Html Msg
navibar page =
    div [class "navibar"]
        [ span [class "navitem", onClick RequestEntryList] [text "一覧"]
        , span [class "navitem"] [text "編集"]
        ]

messageLine : Maybe String -> Html Msg
messageLine maybe_message =
     case maybe_message of
         Just(message) ->
             div [class "messageline"] [text message]
         Nothing  ->
             text ""

topPage : Model -> Html Msg
topPage model =
    text "ようこそ! まずは [一覧] をクリックしてください"

entryListPage : List EntryInfo -> Html Msg
entryListPage entry_list =
    div [class "main_box"]
        [ table [] (List.map (λx -> tr [] [ td [class "id_col"] [text x.id]
                                          , td [class "title_col"] [text x.title]
                                          ]
                             ) (List.sortBy .id entry_list)
                   )
        ]
```

HTMLの構造そのままを Elmのデータ構造で記載しています。ちなみにここで `div` とか `span` とか `class` とかを
当たり前のように使えているのは、パッケージインポートの際

```elm
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
```

しているからです。`(..)` は全部このモジュールの名前空間に取り込んでしまうので、あんまりよくないのですが、
しかし Html周りは 列挙しても大変ですので、これは便利です。

さて、view はあんまり見どころがないのですが、ここの注目ポイントのひとつは、

```elm
[ span [class "navitem", onClick RequestEntryList] [text "一覧"]
```

の部分。onClick という HTMLイベントに Msg を割り当てると、それをした時にイベントが発生するようになる(そしてupdateへ)、というわけです。


もうひとつの注目ポイントは

```
(List.map (λx -> tr [] [ td [class "id_col"] [text x.id]
                       , td [class "title_col"] [text x.title]
                       ]
          ) (List.sortBy .id entry_list)
```

Elm も ラムダは `\` なのですが、実は `λ` も与えられるのです。
'\'を「'λ'の短い棒がかすれちゃった。てへっ♪」と見るのはやっぱり無理があるですし、
なによりは円の呪いに侵されている日本の環境にとっては嬉しい限り。


### subscriptions

この機能は今回使っていませんので、

```
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
```

外部からのコールバックを受け付けたりとかするのが得意なフレンズです。

### Http Request と JSONデコード

updateのところで棚上げした `requestEntryList` です。

```elm
-- REST REQUEST

requestEntryList : Cmd Msg
requestEntryList =
    let
        url = "v1/entries"
    in
        Http.send ShowEntryList (Http.get url entryListDecoder)
```

まぁ、読めばわかると言ったところ。
おわったら ShowEntryList が発火するのはここで指定しているからです。

正常受信時、受け取ったJsonデータをデコードする
`entryListDecoder` はこんな感じ


``` elm
-- JSON DECODER

entryListDecoder : Decode.Decoder (List EntryInfo)
entryListDecoder =
    Decode.field "entries" (Decode.list entryInfoDecoder)


entryInfoDecoder : Decode.Decoder EntryInfo
entryInfoDecoder = 
    Decode.map2 EntryInfo
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
```

`Decode` は、`Json.Decode` を `import Json.Decode as Decode` してるので。


ココらへんについては、Elm公式の [example/http](http://elm-lang.org/examples/http) を見て癒やされるが良いでしょう。


