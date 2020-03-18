port module Main exposing (Model, init, main)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- PORT


port saveAuth : E.Value -> Cmd msg


port requestNews : E.Value -> Cmd msg


port filmRequest : E.Value -> Cmd msg


port videoSourceRequest : E.Value -> Cmd msg


port loadAuth : (E.Value -> msg) -> Sub msg



-- port loadNews : (E.Value -> msg) -> Sub msg


port newsResponse : (E.Value -> msg) -> Sub msg


port filmResponse : (E.Value -> msg) -> Sub msg


port videoPlayerSource : (E.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadAuth LoadAuth

        -- , loadNews LoadNews
        , newsResponse GotNews
        , filmResponse GotFilm
        , videoPlayerSource PlayVideo
        ]



-- MODEL


type alias AuthResult =
    { authToken : String
    , expires : String
    , isFirstLogin : Bool
    }


type alias Credentials =
    { email : String
    , password : String
    , token : Maybe String
    }


type alias Model =
    { credentials : Credentials
    , request : Request
    , alertVisibility : Alert.Visibility
    , news : List News
    , player : Player
    , navbarState : Navbar.State
    }


type Player
    = Initial
    | Source String
    | Error String


type Request
    = NotSentYet
    | Failure Reason
    | Loading
    | Success


type alias News =
    { title : String
    , fileId : Int
    , source : String
    }


type Reason
    = Unauthorized
    | Other String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( Model
        { email = ""
        , password = ""
        , token = Nothing
        }
        NotSentYet
        Alert.closed
        []
        Initial
        navbarState
    , navbarCmd
    )


obtainToken : String -> String -> Cmd Msg
obtainToken email password =
    Http.post
        { url = "http://3.120.74.192:9090/rest/authentication"
        , body = Http.jsonBody (encodeRequestBody email password)
        , expect = Http.expectJson GotToken decodeAuthResult
        }



-- getNews : String -> Cmd Msg
-- getNews token =
--     Http.request
--         { method = "GET"
--         , headers = headers token
--         , url = "http://3.120.74.192:9090/rest/news"
--         , body = Http.jsonBody (E.object [])
--         , expect = Http.expectJson GotNews decodeNewsList
--         , timeout = Nothing
--         , tracker = Nothing
--         }
--


headers : String -> List Http.Header
headers token =
    [ Http.header "Content-Type" "application/json"
    , Http.header "Authorization" token
    , Http.header "Access-Control-Allow-Origin" "*"
    , Http.header "Origin" "*"
    ]


decodeFilm : D.Decoder (Maybe Int)
decodeFilm =
    D.map List.head
        (D.list (D.field "id" D.int))


decodeNewsList : D.Decoder (List News)
decodeNewsList =
    D.list decodeNews


decodeNews : D.Decoder News
decodeNews =
    D.map3 News
        (D.field "title" D.string)
        (D.field "fileId" D.int)
        (D.field "source" D.string)


encodeNewsList : List News -> E.Value
encodeNewsList news =
    E.list encodeNews news


encodeNews : News -> E.Value
encodeNews news =
    E.object
        [ ( "title", E.string news.title )
        , ( "fileId", E.int news.fileId )
        ]


decodeAuthResult : D.Decoder AuthResult
decodeAuthResult =
    D.map3 AuthResult
        (D.field "authToken" D.string)
        (D.field "expires" D.string)
        (D.field "isFirstLogin" D.bool)


encodeAuthResult : AuthResult -> E.Value
encodeAuthResult authResult =
    E.object
        [ ( "authToken", E.string authResult.authToken )
        , ( "expires", E.string authResult.expires )
        , ( "isFirstLogin", E.bool authResult.isFirstLogin )
        ]


encodeRequestBody : String -> String -> E.Value
encodeRequestBody email password =
    E.object
        [ ( "email", E.string email )
        , ( "password", E.string password )
        ]



-- UPDATE


type Msg
    = InputEmail String
    | InputPassword String
    | SignIn
    | SignOut
    | GotToken (Result Http.Error AuthResult)
      -- | GotNews (Result Http.Error (List News))
    | AlertMsg Alert.Visibility
    | LoadAuth E.Value
      -- | LoadNews E.Value
    | PlayVideo E.Value
    | Play Int
    | GotNews E.Value
    | GotFilm E.Value
    | RefreshPlaylist
    | NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email ->
            let
                credentials =
                    model.credentials
            in
            ( { model | credentials = { credentials | email = email } }, Cmd.none )

        InputPassword password ->
            let
                credentials =
                    model.credentials
            in
            ( { model | credentials = { credentials | password = password } }, Cmd.none )

        SignIn ->
            ( { model | alertVisibility = Alert.closed }
            , obtainToken model.credentials.email model.credentials.password
            )

        SignOut ->
            let
                credentials =
                    model.credentials

                emptyAuth =
                    E.object [ ( "status", E.string "Unauthorized" ) ]
            in
            ( { model
                | credentials =
                    { credentials
                        | token = Nothing
                    }
              }
            , saveAuth emptyAuth
            )

        AlertMsg visibility ->
            ( { model | alertVisibility = visibility }, Cmd.none )

        GotToken response ->
            handleAuthResponse response model

        -- GotNews response ->
        --     ( { model
        --         | news =
        --             handleNewsResponse response
        --       }
        --     , Cmd.none
        --     )
        LoadAuth encoded ->
            let
                authResult =
                    D.decodeValue
                        decodeAuthResult
                        encoded

                token =
                    case authResult of
                        Ok result ->
                            Just result.authToken

                        Err _ ->
                            Nothing

                credentials =
                    model.credentials
            in
            ( { model
                | credentials =
                    { credentials | token = token }
              }
            , Cmd.none
            )

        -- LoadNews encoded ->
        --     let
        --         result =
        --             D.decodeValue decodeNewsList encoded
        --
        --         news =
        --             case result of
        --                 Ok value ->
        --                     value
        --
        --                 Err reason ->
        --                     []
        --     in
        --     ( { model | news = news }, Cmd.none )
        PlayVideo encoded ->
            let
                player =
                    case
                        D.decodeValue
                            (D.field "link" D.string)
                            encoded
                    of
                        Ok link ->
                            Source link

                        Err error ->
                            Error <| D.errorToString error
            in
            ( { model
                | player =
                    player
              }
            , Cmd.none
            )

        Play fileId ->
            ( model
            , filmRequest <|
                E.string <|
                    String.fromInt
                        fileId
            )

        GotNews encoded ->
            let
                news =
                    case D.decodeValue decodeNewsList encoded of
                        Ok value ->
                            value

                        Err _ ->
                            []
            in
            ( { model | news = news }
            , Cmd.none
            )

        GotFilm encoded ->
            ( model
            , case D.decodeValue decodeFilm encoded of
                Ok value ->
                    case value of
                        Just filmId ->
                            videoSourceRequest <|
                                E.string <|
                                    String.fromInt
                                        filmId

                        Nothing ->
                            let
                                log =
                                    Debug.log "No Films found" ""
                            in
                            -- TODO Alert about empty data
                            Cmd.none

                Err message ->
                    let
                        log =
                            Debug.log "Film decode error" message
                    in
                    -- TODO Alert about decoder failure
                    Cmd.none
            )

        RefreshPlaylist ->
            ( model
            , Cmd.batch
                [ case model.credentials.token of
                    Just token ->
                        requestNews <| E.string token

                    Nothing ->
                        Cmd.none
                , filmRequest <|
                    E.string <|
                        String.fromInt
                            90
                ]
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )


handleNewsResponse : Result Http.Error (List News) -> List News
handleNewsResponse response =
    case response of
        Ok news ->
            news

        Err error ->
            []


handleAuthResponse : Result Http.Error AuthResult -> Model -> ( Model, Cmd Msg )
handleAuthResponse response model =
    case response of
        Ok authResult ->
            let
                token =
                    authResult.authToken

                credentials =
                    model.credentials
            in
            ( { model
                | request = Success
                , credentials = { credentials | token = Just token }
                , alertVisibility = Alert.closed
              }
            , Cmd.batch
                [ saveAuth <| encodeAuthResult authResult
                , requestNews <| E.string token
                ]
            )

        Err error ->
            case error of
                Http.BadUrl url ->
                    ( { model
                        | request = Failure <| Other <| "Bad url: " ++ url
                        , alertVisibility = Alert.shown
                      }
                    , Cmd.none
                    )

                Http.Timeout ->
                    ( { model
                        | request = Failure <| Other "Request timeout"
                        , alertVisibility = Alert.shown
                      }
                    , Cmd.none
                    )

                Http.NetworkError ->
                    ( { model
                        | request = Failure <| Other "Network error"
                        , alertVisibility = Alert.shown
                      }
                    , Cmd.none
                    )

                Http.BadStatus code ->
                    handleStatusCode code model

                Http.BadBody _ ->
                    ( { model
                        | request = Failure <| Other "Unexpected content received"
                        , alertVisibility = Alert.shown
                      }
                    , Cmd.none
                    )


handleStatusCode : Int -> Model -> ( Model, Cmd Msg )
handleStatusCode code model =
    case code of
        401 ->
            ( { model
                | request = Failure Unauthorized
                , alertVisibility = Alert.shown
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.credentials.token of
        Just token ->
            div []
                [ viewNavbar model
                , viewAdmin model
                ]

        Nothing ->
            viewSignIn model


viewNavbar : Model -> Html Msg
viewNavbar model =
    Grid.container []
        -- Wrap in a container to center the navbar
        [ Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.collapseMedium
            -- Collapse menu at the medium breakpoint
            |> Navbar.info
            -- Customize coloring
            -- |> Navbar.brand
            --     -- Add logo to your brand with a little styling to align nicely
            --     [ href "#" ]
            --     [ img
            --         [ src "assets/images/elm-bootstrap.svg"
            --         , class "d-inline-block align-top"
            --         , style [ ( "width", "30px" ) ]
            --         ]
            --         []
            --     , text " Elm Bootstrap"
            --     ]
            -- |> Navbar.items
            --     [ Navbar.itemLink
            --         [ href "#" ]
            --         [ text "Item 1" ]
            --     , Navbar.dropdown
            --         -- Adding dropdowns is pretty simple
            --         { id = "mydropdown"
            --         , toggle = Navbar.dropdownToggle [] [ text "My dropdown" ]
            --         , items =
            --             [ Navbar.dropdownHeader [ text "Heading" ]
            --             , Navbar.dropdownItem
            --                 [ href "#" ]
            --                 [ text "Drop item 1" ]
            --             , Navbar.dropdownItem
            --                 [ href "#" ]
            --                 [ text "Drop item 2" ]
            --             , Navbar.dropdownDivider
            --             , Navbar.dropdownItem
            --                 [ href "#" ]
            --                 [ text "Drop item 3" ]
            --             ]
            --         }
            --     ]
            |> Navbar.customItems
                [ Navbar.formItem []
                    [ Button.button
                        [ Button.warning
                        , Button.attrs [ Spacing.ml2Sm ]
                        , Button.onClick SignOut
                        ]
                        [ text "Sign out" ]
                    ]
                ]
            |> Navbar.view model.navbarState
        ]


viewAdmin : Model -> Html Msg
viewAdmin model =
    Grid.container []
        [ Grid.row []
            [ Grid.col
                []
                [ Card.config []
                    |> Card.block []
                        [ Block.text []
                            [ div []
                                [ Button.button
                                    [ Button.secondary
                                    , Button.onClick RefreshPlaylist
                                    ]
                                    [ text "Refresh" ]
                                ]
                            ]
                        , Block.text []
                            [ ListGroup.custom <|
                                List.map viewNews model.news
                            ]
                        ]
                    |> Card.view
                ]
            , Grid.col
                []
                [ case model.player of
                    Initial ->
                        viewHelpText

                    Source video ->
                        videoPlayer video

                    Error message ->
                        viewErrorMessage message
                ]
            ]
        ]


videoPlayer : String -> Html Msg
videoPlayer source =
    div []
        [ div []
            [ video
                [ width 320
                , height 240
                , autoplay True
                , src source
                ]
                []
            ]
        , div
            []
            [ Button.button
                [ Button.success
                , Button.attrs [ Spacing.ml1 ]
                ]
                [ text "Accept" ]
            , Button.button
                [ Button.danger
                , Button.attrs [ Spacing.ml1 ]
                ]
                [ text "Reject" ]
            ]
        ]


viewHelpText : Html Msg
viewHelpText =
    div []
        [ h3 []
            [ text <|
                "Click to one of the"
                    ++ " videos listed in the"
                    ++ " left to play it !"
            ]
        ]


viewErrorMessage : String -> Html Msg
viewErrorMessage message =
    div []
        [ h3 [] [ text message ] ]


viewNews : News -> ListGroup.CustomItem Msg
viewNews news =
    ListGroup.button
        [ ListGroup.primary

        -- , ListGroup.attrs [ onClick (PlayVideo news.source) ]
        , ListGroup.attrs [ onClick (Play news.fileId) ]
        ]
        [ text news.title ]


viewSignIn : Model -> Html Msg
viewSignIn model =
    div []
        [ Alert.config
            |> Alert.warning
            |> Alert.dismissable AlertMsg
            |> Alert.children
                [ Alert.h6 [] [ text (getErrorMessage model.request) ] ]
            |> Alert.view model.alertVisibility
        , Grid.container []
            [ Grid.row [ Row.centerMd, Row.middleXs ]
                [ Grid.col
                    [ Col.sm4 ]
                    [ h3
                        []
                        [ text "Newsable admin" ]
                    , Form.form []
                        [ Form.group []
                            [ InputGroup.config
                                (InputGroup.email <|
                                    viewInput model.request
                                        "email"
                                        model.credentials.email
                                        InputEmail
                                )
                                |> InputGroup.predecessors
                                    [ InputGroup.span [] [ text "@" ] ]
                                |> InputGroup.view
                            ]
                        , Form.group []
                            [ InputGroup.config
                                (InputGroup.password <|
                                    viewInput model.request
                                        "password"
                                        model.credentials.password
                                        InputPassword
                                )
                                |> InputGroup.predecessors
                                    [ InputGroup.span [] [ text "*" ] ]
                                |> InputGroup.view
                            , Form.help [] [ text "Minimum 6 characters" ]
                            ]
                        , Grid.row
                            [ Row.betweenXs ]
                            [ Grid.col []
                                [ Button.button
                                    [ Button.primary
                                    , Button.onClick SignIn
                                    ]
                                    [ text "Sign In" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


getErrorMessage : Request -> String
getErrorMessage request =
    case request of
        Failure type_ ->
            case type_ of
                Other message ->
                    message

                Unauthorized ->
                    "Invalid EMAIL or PASSWORD"

        _ ->
            ""


viewInput : Request -> String -> String -> (String -> Msg) -> List (Input.Option Msg)
viewInput request placeholder value command =
    let
        regularInput =
            [ Input.placeholder placeholder
            , Input.value value
            , Input.onInput command
            ]
    in
    case request of
        Failure type_ ->
            case type_ of
                Unauthorized ->
                    Input.danger :: regularInput

                _ ->
                    regularInput

        _ ->
            regularInput
