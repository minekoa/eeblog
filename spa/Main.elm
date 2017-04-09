import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Request)
import Json.Decode as Decode


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

------------------------------------------------------------
-- MODEL
------------------------------------------------------------

type Page
    = TopPage
    | EntryListPage

type alias EntryInfo =
    { id : String
    , title : String
    }

type alias Model =
    { page : Page
    , entry_list : List EntryInfo
    , message : Maybe String
    }


initModel : Model
initModel =
    { page = TopPage
    , entry_list = []
    , message = Nothing
    }


init : (Model, Cmd Msg)
init =
    ( initModel
    , Cmd.none
    )

------------------------------------------------------------
-- UPDATE
------------------------------------------------------------

type Msg
    = RequestEntryList
    | ShowEntryList (Result Http.Error (List EntryInfo))


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

------------------------------------------------------------
-- VIEW
------------------------------------------------------------

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


------------------------------------------------------------
-- SUBSCRIPTIONS
------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

------------------------------------------------------------
-- OTHERS
------------------------------------------------------------

-- REST REQUEST

requestEntryList : Cmd Msg
requestEntryList =
    let
        url = "v1/entries"
    in
        Http.send ShowEntryList (Http.get url entryListDecoder)


-- JSON DECODER

entryListDecoder : Decode.Decoder (List EntryInfo)
entryListDecoder =
    Decode.field "entries" (Decode.list entryInfoDecoder)


entryInfoDecoder : Decode.Decoder EntryInfo
entryInfoDecoder = 
    Decode.map2 EntryInfo
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)

