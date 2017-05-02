# Markdown をちゃんとレンダリングする

* [example/markdown](http://elm-lang.org/examples/markdown)
* [evancz/elm-markdown at 3.0.2](https://github.com/evancz/elm-markdown/tree/3.0.2)

これを使うと、`Markdown.toHtml` で簡単にHTMLレンダリングが得られます。


## 組み込み方法

elm-package.json の `dependencies` に、elm-markdown を追加します

```
    "dependencies": {
        "elm-lang/core": "5.1.1 <= v < 6.0.0",
        "elm-lang/html": "2.0.0 <= v < 3.0.0",
        "elm-lang/http": "1.0.0 <= v < 3.0.1",
        "evancz/elm-markdown": "3.0.0 <= v < 4.0.0"
    },
```

```elm
import Markdown
```


で、つかいます。

```elm
viewEntryPage : Maybe Entry -> Html Msg
viewEntryPage mb_entry =
    case mb_entry of
        Just( entry ) ->
            div [class "main_box"]
                [ span [] [text entry.id]
                , Markdown.toHtmlWith markdownOptions [] entry.content
                ]
        Nothing ->
            div [class "main_box"][text "ページがありません"]

        ・
        ・
        ・

markdownOptions: Markdown.Options
markdownOptions =
  { githubFlavored = Just { tables = True, breaks = True }
  , defaultHighlighting = Nothing
  , sanitize = False
  , smartypants = False
  }
```

`toHtmlWith` を使っているのは、GFM (Github Flavored Markdown) を使いたいから。各オプションの意味はこちら。

| メンバと型                                               | 意味                                                                                                                                                                                                                            |
|----------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `githubFlavored : Maybe { tables : Bool, breaks : Bool }`| GFMを使うか、使うとしたらどの記法を有効にするか、というオプション。table記法と breaksは、単に改行するだけで行末に &lt;br&gt;が入る機能。(普通のMarkdownは行末に半角スペースを二個入れると改行になる)                            |
| `defaultHighlighting : Maybe String`                     | コードブロックのハイライト、デフォルトだとどんなルールが良い？という機能。その前に　`highlight.js` をアプリにインポートしとけよという話はある. (ぶっちゃけelm-markdownは `highlight.js` のルールで pre要素にクラスつけてるだけ) |
| `sanitize : Bool`                                        | HTMLのエスケープを行うか否か。Markdown自体は「最低限の文法 + HTMLでマークアップ」という戦略なのだけれども、Webアプリではこれはちょっと危ない。そういう選択。                                                                    |
| `smartypants : Bool`                                     | ASCII文字のまま、ASCIIで省略された文字をちゃんと使いたいという欧米人の甘えに応える機能。                                                                                                                                        |

[Markdown.md at 3.0.2 - evancz/elm-markdown](https://github.com/evancz/elm-markdown/blob/3.0.2/src/Markdown.elm)

> ```elm
> {-| Some parser options so you can tweak things for your particular case.
>   * `githubFlavored` &mdash; overall reasonable improvements on the original
>     markdown parser as described [here][gfm]. This includes stuff like [fenced
>     code blocks][fenced]. There are some odd parts though, such as [tables][]
>     and a setting to turn all newlines into newlines in the resulting output,
>     so there are settings to turn those on or off based on your preference.
>   * `defaultHighlighting` &mdash; a default language to use for code blocks that do
>     not have a language tag. So setting this to `Just "elm"` will treat all
>     unlabeled code blocks as Elm code. (This relies on [highlight.js][highlight]
>     as explained in the README [here](../#code-blocks).)
>   * `sanitize` &mdash; this determines if all HTML should be escaped. If you
>     are parsing user markdown or user input can somehow reach the markdown
>     parser, you should almost certainly turn on sanitation. If it is just you
>     writing markdown, turning sanitation off is a nice way to do some HTML
>     tricks if it is needed.
>   * `smartypants` &mdash; This will automatically upgrade quotes to the
>     prettier versions and turn dashes into [em dashes or en dashes][dash]
> [gfm]: https://help.github.com/articles/github-flavored-markdown/
> [fenced]: https://help.github.com/articles/github-flavored-markdown/#fenced-code-blocks
> [tables]: https://help.github.com/articles/github-flavored-markdown/#tables
> [highlight]: https://highlightjs.org/
> [dash]: http://en.wikipedia.org/wiki/Dash
> -}
> type alias Options =
>   { githubFlavored : Maybe { tables : Bool, breaks : Bool }
>   , defaultHighlighting : Maybe String
>   , sanitize : Bool
>   , smartypants : Bool
>   }
> ```


