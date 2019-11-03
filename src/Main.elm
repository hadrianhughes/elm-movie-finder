import Browser
import Html exposing (Html, div, ul, li, input, button, text, img, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode

main =
  Browser.element
    {
      init = init,
      update = update,
      view = view,
      subscriptions = subscriptions
    }


-- MODEL

type alias Movie =
  {
    title : String,
    image : String
  }

type alias Model = 
  {
    term : String,
    results : List Movie
  }


init : () -> (Model, Cmd Msg)
init _ =
  (
    {
      term = "",
      results = []
    },
    Cmd.none
  )


-- UPDATE

type Msg = Change String | Search | GotResults (Result Http.Error (List Movie))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change value ->
      ({ model | term = value }, Cmd.none)
    Search ->
      (model, getMovies model.term)
    GotResults response ->
      case response of
        Ok results ->
          ({ model | results = results }, Cmd.none)
        Err _ ->
          (model, Cmd.none)


api : String -> String
api query = "http://www.omdbapi.com/?apikey=8eff793c" ++ "&s=" ++ query

decodeItem : Json.Decode.Decoder Movie
decodeItem =
  Json.Decode.map2 Movie
    (Json.Decode.field "Title" Json.Decode.string)
    (Json.Decode.field "Poster" Json.Decode.string)

decodeJson : Json.Decode.Decoder (List Movie)
decodeJson =
  Json.Decode.map identity
    (Json.Decode.field "Search" (Json.Decode.list decodeItem))

getMovies : String -> Cmd Msg
getMovies query =
  Http.get
    {
      url = api query,
      expect = Http.expectJson GotResults decodeJson
    }


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "app" ]
    [
      div [ class "input" ]
        [
          input [
            placeholder "Search for a movie...",
            value model.term,
            onInput Change
          ] [],
          button [ onClick Search ] [ text "Search" ]
        ],
      ul [ class "movies" ] <| List.map movieItem model.results
    ]

movieItem : Movie -> Html Msg
movieItem movie =
  li [ class "movie-item" ]
    [
      img [ src movie.image ] [],
      span [] [ text movie.title ]
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
