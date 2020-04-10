port module Main exposing (Model, init, main)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Tab as Tab
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Loading exposing (LoaderType(..), defaultConfig, render)
import Page as Page


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- PORT


port saveAuth : E.Value -> Cmd msg


port loadAuth : (E.Value -> msg) -> Sub msg


port requestNews : E.Value -> Cmd msg


port newsResponse : (E.Value -> msg) -> Sub msg


port filmRequest : E.Value -> Cmd msg


port filmResponse : (E.Value -> msg) -> Sub msg


port videoSourceRequest : E.Value -> Cmd msg


port videoSourceResponse : (E.Value -> msg) -> Sub msg


port createNewsRequest : E.Value -> Cmd msg


port createNewsResponse : (E.Value -> msg) -> Sub msg


port acceptNewsRequest : E.Value -> Cmd msg


port acceptNewsResponse : (E.Value -> msg) -> Sub msg


port rejectNewsRequest : E.Value -> Cmd msg


port rejectNewsResponse : (E.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadAuth LoadAuth
        , newsResponse GotNews
        , filmResponse GotFilm
        , videoSourceResponse PlayVideo
        , createNewsResponse NewsCreationResponse
        , acceptNewsResponse GotAcceptedNews
        , rejectNewsResponse GotRejectedNews
        , Sub.map PageMsg <|
            Page.subscriptions model.newsPage
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
    , editor : Editor
    , navbarState : Navbar.State
    , newsTemplate : CreateNewsTemplate
    , createNewsStatus : CreateNews
    , newsPage : Page.Model
    }


type CreateNews
    = Ready
    | Busy


type alias CreateNewsTemplate =
    { title : String
    , description : String
    , url : String
    }


type Editor
    = Initial
    | LoadingVideo
    | PlayingVideo String
    | Error String
    | Message String
    | AddNews


type Request
    = NotSentYet
    | Failure Reason
    | Loading
    | Success


type Reason
    = Unauthorized
    | Other String



-- INIT


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
        Initial
        navbarState
        clearNewsFormData
        Ready
        Page.init
    , navbarCmd
    )



-- HTTP


obtainToken : String -> String -> Cmd Msg
obtainToken email password =
    Http.post
        { url = "http://3.120.74.192:9090/rest/authentication"
        , body = Http.jsonBody (encodeRequestBody email password)
        , expect = Http.expectJson GotToken decodeAuthResult
        }


handleAuthResponse : Result Http.Error AuthResult -> Model -> ( Model, Cmd Msg )
handleAuthResponse response model =
    case response of
        Ok authResult ->
            let
                token =
                    authResult.authToken

                credentials =
                    model.credentials

                ( newsPage, httpRequestor ) =
                    Page.refreshCurrentPage model.newsPage token requestNews
            in
            ( { model
                | request = Success
                , credentials = { credentials | token = Just token }
                , alertVisibility = Alert.closed
                , newsPage = newsPage
              }
            , Cmd.batch
                [ saveAuth <| encodeAuthResult authResult
                , httpRequestor
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



-- ENCODE DECODE


decodeAddNewsResponse : D.Decoder String
decodeAddNewsResponse =
    D.field "statusCode" D.string


encodeNewsTemplate : CreateNewsTemplate -> E.Value
encodeNewsTemplate template =
    E.object
        [ ( "title", E.string template.title )
        , ( "description", E.string template.description )
        , ( "url", E.string template.url )
        ]


decodeFilm : D.Decoder (Maybe Int)
decodeFilm =
    D.map List.head
        (D.list (D.field "id" D.int))


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
    | AlertMsg Alert.Visibility
    | LoadAuth E.Value
    | PlayVideo E.Value
    | GotNews E.Value
    | GotFilm E.Value
    | NavbarMsg Navbar.State
    | CloseNewsCreator
    | ClearNewsFormData
    | InputNewsTitle String
    | InputNewsDescription String
    | InputNewsUrl String
    | CreateNews
    | NewsCreationResponse E.Value
    | AcceptNews
    | RejectNews
    | GotAcceptedNews E.Value
    | GotRejectedNews E.Value
    | PageMsg Page.Msg


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

        LoadAuth encoded ->
            let
                ( authToken, newsPage, cmd ) =
                    case
                        D.decodeValue
                            decodeAuthResult
                            encoded
                    of
                        Ok auth ->
                            let
                                token =
                                    auth.authToken

                                ( page, params ) =
                                    Page.loadFirstPage model.newsPage token
                            in
                            ( Just token
                            , page
                            , requestNews <|
                                params
                            )

                        Err _ ->
                            ( Nothing, model.newsPage, Cmd.none )

                credentials =
                    model.credentials
            in
            ( { model
                | credentials =
                    { credentials | token = authToken }
                , newsPage = newsPage
              }
            , cmd
            )

        PlayVideo encoded ->
            let
                editor =
                    case
                        D.decodeValue
                            (D.field "url" D.string)
                            encoded
                    of
                        Ok url ->
                            PlayingVideo url

                        Err error ->
                            Error <| D.errorToString error
            in
            ( { model
                | editor =
                    editor
              }
            , Cmd.none
            )

        GotNews encoded ->
            let
                ( page, value ) =
                    case model.credentials.token of
                        Just token ->
                            Page.handleIncomingNews encoded token model.newsPage

                        Nothing ->
                            ( model.newsPage, Nothing )
            in
            ( { model | newsPage = page }
            , case value of
                Just params ->
                    requestNews params

                Nothing ->
                    Cmd.none
            )

        GotFilm encoded ->
            ( model
            , case D.decodeValue decodeFilm encoded of
                Ok value ->
                    case value of
                        Just filmId ->
                            case model.credentials.token of
                                Just token ->
                                    videoSourceRequest <|
                                        E.object
                                            [ ( "fileId"
                                              , E.string <|
                                                    String.fromInt
                                                        filmId
                                              )
                                            , ( "token", E.string token )
                                            ]

                                Nothing ->
                                    Cmd.none

                        Nothing ->
                            -- -- TODO Alert about empty data
                            Cmd.none

                Err message ->
                    -- TODO Alert about decoder failure
                    Cmd.none
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        CloseNewsCreator ->
            ( { model | editor = Initial }, Cmd.none )

        ClearNewsFormData ->
            ( { model
                | editor = AddNews
                , newsTemplate = clearNewsFormData
              }
            , Cmd.none
            )

        InputNewsTitle title ->
            let
                origTemplate =
                    model.newsTemplate
            in
            ( { model
                | newsTemplate =
                    { origTemplate | title = title }
              }
            , Cmd.none
            )

        InputNewsDescription description ->
            let
                origTemplate =
                    model.newsTemplate
            in
            ( { model
                | newsTemplate =
                    { origTemplate | description = description }
              }
            , Cmd.none
            )

        InputNewsUrl url ->
            let
                origTemplate =
                    model.newsTemplate
            in
            ( { model
                | newsTemplate =
                    { origTemplate | url = url }
              }
            , Cmd.none
            )

        CreateNews ->
            ( { model | createNewsStatus = Busy }
            , case model.credentials.token of
                Just token ->
                    createNewsRequest <|
                        E.object
                            [ ( "news", encodeNewsTemplate model.newsTemplate )
                            , ( "token", E.string token )
                            ]

                Nothing ->
                    Cmd.none
            )

        NewsCreationResponse encoded ->
            let
                responseStatus =
                    case D.decodeValue decodeAddNewsResponse encoded of
                        Ok status ->
                            if status == "OK" then
                                Error "News successfuly added"

                            else
                                Error "Could not add news"

                        Err _ ->
                            Error "Could not add news"
            in
            ( { model
                | editor = responseStatus
                , createNewsStatus = Ready
                , newsTemplate = clearNewsFormData
              }
            , Cmd.none
            )

        AcceptNews ->
            ( model
            , case model.newsPage.selected of
                Just news ->
                    case model.credentials.token of
                        Just token ->
                            acceptNewsRequest <|
                                E.object
                                    [ ( "newsId"
                                      , E.string <|
                                            String.fromInt news.id
                                      )
                                    , ( "token", E.string token )
                                    ]

                        Nothing ->
                            Cmd.none

                Nothing ->
                    -- TODO Tell user about this case
                    Cmd.none
            )

        RejectNews ->
            ( model
            , case model.newsPage.selected of
                Just news ->
                    case model.credentials.token of
                        Just token ->
                            rejectNewsRequest <|
                                E.object
                                    [ ( "newsId"
                                      , E.string <|
                                            String.fromInt news.id
                                      )
                                    , ( "token", E.string token )
                                    ]

                        Nothing ->
                            Cmd.none

                Nothing ->
                    -- TODO tell user about this case
                    Cmd.none
            )

        GotAcceptedNews encoded ->
            let
                editor =
                    case
                        D.decodeValue
                            (D.field "statusCode" D.string)
                            encoded
                    of
                        Ok message ->
                            if message == "NO_CONTENT" then
                                Message "Accepted"

                            else
                                Error "Failed to accept"

                        Err message ->
                            Error <| D.errorToString message
            in
            ( { model | editor = editor }, Cmd.none )

        GotRejectedNews encoded ->
            let
                editor =
                    case
                        D.decodeValue
                            (D.field "statusCode" D.string)
                            encoded
                    of
                        Ok message ->
                            if message == "NO_CONTENT" then
                                Message "Rejected"

                            else
                                Error "Failed to reject"

                        Err err ->
                            Error <| D.errorToString err
            in
            ( { model | editor = editor }, Cmd.none )

        PageMsg pageMsg ->
            let
                ( page, pageCmd ) =
                    Page.update pageMsg
                        model.newsPage
                        requestNews
                        model.credentials.token

                ( editor, editorCmd ) =
                    case pageMsg of
                        Page.Play news ->
                            case model.credentials.token of
                                Just token ->
                                    ( LoadingVideo
                                    , filmRequest <|
                                        E.object
                                            [ ( "filmId"
                                              , E.string <|
                                                    String.fromInt news.fileId
                                              )
                                            , ( "token", E.string token )
                                            ]
                                    )

                                Nothing ->
                                    ( model.editor, Cmd.none )

                        Page.OpenNewsCreator ->
                            ( AddNews, Cmd.none )

                        _ ->
                            ( model.editor, Cmd.none )
            in
            ( { model
                | newsPage = page
                , editor = editor
              }
            , Cmd.batch [ pageCmd, editorCmd ]
            )



-- HELPER


clearNewsFormData : CreateNewsTemplate
clearNewsFormData =
    { title = ""
    , description = ""
    , url = ""
    }



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
                [ Html.map PageMsg <|
                    Page.view model.newsPage
                ]
            , Grid.col [] [ viewEditor model ]
            ]
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    case model.editor of
        Initial ->
            Card.config [ Card.attrs [ width 20 ] ]
                |> Card.block []
                    [ Block.text [ class "text-center" ]
                        [ h3 [ Spacing.mt2 ]
                            [ text <|
                                "Click to one of the"
                                    ++ " videos listed in the"
                                    ++ " left to play it !"
                            ]
                        ]
                    ]
                |> Card.view

        LoadingVideo ->
            Card.config [ Card.attrs [ width 20 ] ]
                |> Card.header [ class "text-center" ]
                    [ h4 []
                        [ case model.newsPage.selected of
                            Just news ->
                                text news.title

                            Nothing ->
                                text "Video has no title"
                        ]
                    ]
                |> Card.block []
                    [ Block.text [ class "text-center" ]
                        [ Loading.render
                            Loading.Spinner
                            -- LoaderType
                            { defaultConfig
                                | color = "#d3869b"
                                , size = 150
                            }
                            -- Config
                            Loading.On
                        ]
                    ]
                |> Card.view

        PlayingVideo url ->
            let
                ( video, actions ) =
                    videoPlayer url model.newsPage.selected
            in
            Card.config
                [ Card.align Text.alignXsCenter
                , Card.attrs [ width 20 ]
                ]
                |> Card.header [ class "text-center" ]
                    [ h4 []
                        [ case model.newsPage.selected of
                            Just news ->
                                text news.title

                            Nothing ->
                                text "Video has no title"
                        ]
                    ]
                |> Card.block [] [ Block.text [] [ video ] ]
                |> Card.footer [] [ actions ]
                |> Card.view

        Error message ->
            Card.config [ Card.attrs [ width 20 ] ]
                |> Card.header [ class "text-center" ]
                    [ h3 [ Spacing.mt2 ] [ text message ] ]
                |> Card.view

        Message message ->
            Card.config [ Card.attrs [ width 20 ] ]
                |> Card.header [ class "text-center" ]
                    [ h3 [ Spacing.mt2 ] [ text message ] ]
                |> Card.view

        AddNews ->
            Card.config [ Card.attrs [ width 20 ] ]
                |> Card.header [ class "text-center" ]
                    [ viewAddNews model ]
                |> Card.view


videoPlayer : String -> Maybe Page.News -> ( Html Msg, Html Msg )
videoPlayer url value =
    let
        actionButton : String -> Msg -> Button.Option Msg -> Attribute Msg -> Html Msg
        actionButton name msg color attribute =
            case value of
                Just news ->
                    Button.button
                        (color
                            :: [ Button.attrs [ attribute ]
                               , Button.onClick msg
                               ]
                        )
                        [ text name ]

                Nothing ->
                    Button.button
                        (color
                            :: [ Button.disabled True
                               , Button.attrs [ attribute ]
                               , Button.onClick msg
                               ]
                        )
                        [ text name ]
    in
    ( div []
        [ video
            [ width 320
            , height 240
            , autoplay True
            , src url
            ]
            []
        ]
    , div []
        [ actionButton "Accept" AcceptNews Button.success Spacing.mr3
        , actionButton "Reject" RejectNews Button.danger Spacing.ml3
        ]
    )


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


viewAddNews : Model -> Html Msg
viewAddNews model =
    Form.form []
        [ Form.group []
            [ Form.label [ for "title" ] [ text "Title" ]
            , Input.text
                [ Input.id "title"
                , Input.onInput InputNewsTitle
                , Input.value model.newsTemplate.title
                ]
            ]
        , Form.group []
            [ label [ for "description" ] [ text "Description" ]
            , Textarea.textarea
                [ Textarea.id "description"
                , Textarea.rows 3
                , Textarea.onInput InputNewsDescription
                , Textarea.value model.newsTemplate.description
                ]
            ]
        , Form.group []
            [ Form.label [ for "url" ] [ text "Url" ]
            , Input.text
                [ Input.id "url"
                , Input.onInput InputNewsUrl
                , Input.value model.newsTemplate.url
                ]
            ]
        , viewSaveButton model
        , Button.button
            [ Button.warning
            , Button.attrs [ Spacing.ml1 ]
            , Button.onClick ClearNewsFormData
            ]
            [ text "Clear form" ]
        , Button.button
            [ Button.warning
            , Button.attrs [ Spacing.ml1 ]
            , Button.onClick CloseNewsCreator
            ]
            [ text "Close" ]
        ]


viewSaveButton : Model -> Html Msg
viewSaveButton model =
    case model.createNewsStatus of
        Ready ->
            viewAddNewsButton model.newsTemplate

        Busy ->
            viewBusyButton


viewAddNewsButton : CreateNewsTemplate -> Html Msg
viewAddNewsButton template =
    if
        template.title
            == ""
            || template.description
            == ""
            || template.url
            == ""
    then
        Button.button
            [ Button.primary
            , Button.disabled True
            , Button.attrs [ Spacing.ml1 ]
            ]
            [ text "Add news" ]

    else
        Button.button
            [ Button.primary
            , Button.attrs [ Spacing.ml1 ]
            , Button.onClick CreateNews
            ]
            [ text "Add news" ]


viewBusyButton : Html Msg
viewBusyButton =
    Button.button
        [ Button.primary
        , Button.disabled True
        , Button.attrs [ Spacing.mr1 ]
        ]
        [ Spinner.spinner
            [ Spinner.small
            , Spinner.color Text.warning
            , Spinner.attrs [ Spacing.mr1 ]
            ]
            []
        , text "Saving..."
        ]
