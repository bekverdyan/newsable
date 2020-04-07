module Page exposing
    ( Msg(..)
    , News
    , State
    , handleIncomingNews
    , init
    , loadFirstPage
    , refreshCurrentPage
    , subscriptions
    , toAcceptedNewsTab
    , toAllNewsTab
    , toNextPage
    , toPreviousPage
    , toRejectedNewsTab
    , toStartPage
    , update
    , view
    )

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Spinner as Spinner
import Bootstrap.Tab as Tab
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias State =
    { start : Int
    , count : Int
    , previous : Page
    , current : Page
    , next : Page
    , type_ : Type
    , requestor : Requestor
    , selected : Maybe News
    , tabState : Tab.State
    }


type Page
    = Empty
    | Error String
    | Data (List News)


type Type
    = All
    | Accepted
    | Rejected


type Requestor
    = Start
    | Previous
    | Current
    | Next
    | NoOne


type alias News =
    { id : Int
    , title : String
    , fileId : Int
    , accepted : Bool
    }


init : State
init =
    { start = 0
    , count = 15
    , previous = Empty
    , current = Empty
    , next = Empty
    , type_ = All
    , requestor = Current
    , selected = Nothing
    , tabState = Tab.customInitialState "allTab"
    }


handleIncomingNews : E.Value -> String -> State -> ( State, Maybe E.Value )
handleIncomingNews encoded token pageOrig =
    case pageOrig.requestor of
        Current ->
            let
                page =
                    applyNews encoded pageOrig
            in
            ( { page | requestor = Next }
            , Just
                (encodeHttpRequest
                    (toQueryString
                        (page.start + page.count)
                        page.count
                        page.type_
                    )
                    token
                )
            )

        Start ->
            let
                page =
                    applyNews encoded pageOrig
            in
            ( { page | requestor = Next }
            , Just
                (encodeHttpRequest
                    (toQueryString
                        (page.start + page.count)
                        page.count
                        page.type_
                    )
                    token
                )
            )

        _ ->
            ( applyNews encoded pageOrig, Nothing )


toQueryString : Int -> Int -> Type -> String
toQueryString start count type_ =
    "?start="
        ++ String.fromInt start
        ++ "&count="
        ++ String.fromInt count
        ++ ("&accepted="
                ++ (case type_ of
                        Accepted ->
                            "true"

                        Rejected ->
                            "false"

                        All ->
                            "all"
                   )
           )


encodeHttpRequest : String -> String -> E.Value
encodeHttpRequest query token =
    E.object
        [ ( "query", E.string query )
        , ( "token", E.string token )
        ]


applyNews : E.Value -> State -> State
applyNews encoded page =
    let
        decoded =
            D.decodeValue decodeNewsList encoded

        toString : D.Error -> Page
        toString error =
            Error <| D.errorToString error
    in
    case page.requestor of
        Start ->
            case decoded of
                Ok news ->
                    { page
                        | current = Data news
                        , requestor = NoOne
                    }

                Err err ->
                    { page
                        | current = toString err
                        , requestor = NoOne
                    }

        Previous ->
            case decoded of
                Ok news ->
                    { page
                        | previous = Data news
                        , requestor = NoOne
                    }

                Err err ->
                    { page
                        | previous = toString err
                        , requestor = NoOne
                    }

        Current ->
            case decoded of
                Ok news ->
                    { page
                        | current = Data news
                        , requestor = NoOne
                    }

                Err err ->
                    { page
                        | current = toString err
                        , requestor = NoOne
                    }

        Next ->
            case decoded of
                Ok news ->
                    { page
                        | next =
                            if List.isEmpty news then
                                Empty

                            else
                                Data news
                        , requestor = NoOne
                    }

                Err err ->
                    { page
                        | next = toString err
                        , requestor = NoOne
                    }

        NoOne ->
            page


decodeNewsList : D.Decoder (List News)
decodeNewsList =
    D.list decodeNews


decodeNews : D.Decoder News
decodeNews =
    D.map4 News
        (D.field "id" D.int)
        (D.field "title" D.string)
        (D.field "fileId" D.int)
        (D.field "accepted" D.bool)


loadFirstPage : State -> String -> ( State, E.Value )
loadFirstPage state token =
    ( { state | start = 0, requestor = Start }
    , encodeHttpRequest
        (toQueryString
            0
            state.count
            state.type_
        )
        token
    )


refreshCurrentPage : State -> String -> (E.Value -> Cmd msg) -> ( State, Cmd msg )
refreshCurrentPage state token requestNews =
    ( { state | requestor = Current }
    , requestNews <|
        encodeHttpRequest
            (toQueryString
                state.start
                state.count
                state.type_
            )
            token
    )


toAllNewsTab : State -> String -> (E.Value -> Cmd msg) -> ( State, Cmd msg )
toAllNewsTab model token requestNews =
    toStartPage
        { model
            | type_ = All
            , tabState = Tab.customInitialState "allTab"
        }
        token
        requestNews


toAcceptedNewsTab : State -> String -> (E.Value -> Cmd msg) -> ( State, Cmd msg )
toAcceptedNewsTab state token requestNews =
    toStartPage
        { state
            | type_ = Accepted
            , tabState = Tab.customInitialState "acceptedTab"
        }
        token
        requestNews


toRejectedNewsTab : State -> String -> (E.Value -> Cmd msg) -> ( State, Cmd msg )
toRejectedNewsTab state token requestNews =
    toStartPage
        { state
            | type_ = Rejected
            , tabState = Tab.customInitialState "rejectedTab"
        }
        token
        requestNews


toStartPage : State -> String -> (E.Value -> Cmd msg) -> ( State, Cmd msg )
toStartPage state token requestNews =
    ( State
        0
        state.count
        Empty
        Empty
        Empty
        state.type_
        Start
        state.selected
        state.tabState
    , requestNews <|
        encodeHttpRequest
            (toQueryString
                0
                state.count
                state.type_
            )
            token
    )


toPreviousPage : State -> String -> (E.Value -> Cmd msg) -> ( State, Cmd msg )
toPreviousPage state token requestNews =
    let
        ( requestor, cmd ) =
            case state.previous of
                Empty ->
                    ( NoOne, Cmd.none )

                _ ->
                    ( Previous
                    , requestNews <|
                        encodeHttpRequest
                            (toQueryString
                                (state.start - (state.count * 2))
                                state.count
                                state.type_
                            )
                            token
                    )
    in
    ( State
        (state.start - state.count)
        state.count
        Empty
        state.previous
        state.current
        state.type_
        requestor
        state.selected
        state.tabState
    , cmd
    )


toNextPage : State -> String -> (E.Value -> Cmd msg) -> ( State, Cmd msg )
toNextPage state token requestNews =
    let
        ( requestor, cmd ) =
            case state.next of
                Empty ->
                    ( NoOne, Cmd.none )

                _ ->
                    ( Next
                    , requestNews <|
                        encodeHttpRequest
                            (toQueryString
                                (state.start
                                    + (state.count * 2)
                                )
                                state.count
                                state.type_
                            )
                            token
                    )
    in
    ( State
        (state.start + state.count)
        state.count
        state.current
        state.next
        Empty
        state.type_
        Next
        state.selected
        state.tabState
    , cmd
    )


type Msg
    = OpenNewsCreator
    | RefreshPlaylist
    | TabMsg Tab.State
    | ToAllNewsTab
    | Play News
    | ToAcceptedNewsTab
    | ToRejectedNewsTab
    | ToStartPage
    | ToPreviousPage
    | ToNextPage



-- UPDATE


update :
    Msg
    -> State
    -> (E.Value -> Cmd msg)
    -> Maybe String
    -> ( State, Cmd msg )
update message model requestNews auth =
    case message of
        RefreshPlaylist ->
            case auth of
                Just token ->
                    refreshCurrentPage
                        model
                        token
                        requestNews

                Nothing ->
                    ( model, Cmd.none )

        ToStartPage ->
            case auth of
                Just token ->
                    toStartPage model token requestNews

                Nothing ->
                    ( model, Cmd.none )

        ToPreviousPage ->
            case auth of
                Just token ->
                    toPreviousPage model token requestNews

                Nothing ->
                    ( model, Cmd.none )

        ToNextPage ->
            case auth of
                Just token ->
                    toNextPage model token requestNews

                Nothing ->
                    ( model, Cmd.none )

        TabMsg state ->
            ( { model | tabState = state }, Cmd.none )

        ToAllNewsTab ->
            case auth of
                Just token ->
                    toAllNewsTab model token requestNews

                Nothing ->
                    ( model, Cmd.none )

        ToAcceptedNewsTab ->
            case auth of
                Just token ->
                    toAcceptedNewsTab model token requestNews

                Nothing ->
                    ( model, Cmd.none )

        ToRejectedNewsTab ->
            case auth of
                Just token ->
                    toRejectedNewsTab model token requestNews

                Nothing ->
                    ( model, Cmd.none )

        Play news ->
            ( { model | selected = Just news }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : State -> Html Msg
view model =
    Card.config
        [ Card.align Text.alignXsCenter ]
        |> Card.header []
            [ div []
                [ viewRefreshButton model.requestor
                , Button.button
                    [ Button.primary
                    , Button.attrs [ Spacing.ml3 ]
                    , Button.onClick OpenNewsCreator
                    ]
                    [ text "Create news" ]
                ]
            ]
        |> Card.block []
            [ Block.text []
                [ Tab.config TabMsg
                    |> Tab.withAnimation
                    |> Tab.center
                    |> Tab.items
                        [ Tab.item
                            { id = "allTab"
                            , link =
                                Tab.link
                                    [ case model.requestor of
                                        NoOne ->
                                            onClick ToAllNewsTab

                                        _ ->
                                            disabled True
                                    ]
                                    [ text "All"
                                    ]
                            , pane =
                                Tab.pane []
                                    [ viewTabContent
                                        model
                                    ]
                            }
                        , Tab.item
                            { id = "acceptedTab"
                            , link =
                                Tab.link
                                    [ case model.requestor of
                                        NoOne ->
                                            onClick ToAcceptedNewsTab

                                        _ ->
                                            disabled True
                                    ]
                                    [ text "Accepted" ]
                            , pane =
                                Tab.pane []
                                    [ viewTabContent
                                        model
                                    ]
                            }
                        , Tab.item
                            { id = "rejectedTab"
                            , link =
                                Tab.link
                                    [ case model.requestor of
                                        NoOne ->
                                            onClick ToRejectedNewsTab

                                        _ ->
                                            disabled True
                                    ]
                                    [ text "Rejected" ]
                            , pane =
                                Tab.pane []
                                    [ viewTabContent
                                        model
                                    ]
                            }
                        ]
                    |> Tab.view model.tabState
                ]
            ]
        |> Card.footer []
            [ div []
                [ viewStart model
                , viewPrevious model
                , viewNext model
                ]
            ]
        |> Card.view


viewRefreshButton : Requestor -> Html Msg
viewRefreshButton requestor =
    case requestor of
        NoOne ->
            Button.button
                [ Button.primary
                , Button.attrs [ Spacing.mr3 ]
                , Button.onClick RefreshPlaylist
                ]
                [ text "Refresh" ]

        Current ->
            Button.button
                [ Button.primary
                , Button.disabled True
                , Button.attrs [ Spacing.mr3 ]
                ]
                [ Spinner.spinner
                    [ Spinner.small
                    , Spinner.color Text.warning
                    , Spinner.attrs [ Spacing.mr1 ]
                    ]
                    []
                , text "Loading..."
                ]

        _ ->
            Button.button
                [ Button.primary
                , Button.attrs [ Spacing.mr3 ]
                , Button.disabled True
                ]
                [ text "Refresh" ]


viewTabContent : State -> Html Msg
viewTabContent model =
    case model.requestor of
        Current ->
            Spinner.spinner
                [ Spinner.color Text.dark
                , Spinner.attrs
                    [ style "width" "5rem"
                    , style "height" "5rem"
                    ]
                ]
                []

        Start ->
            Spinner.spinner
                [ Spinner.color Text.dark
                , Spinner.attrs
                    [ style "width" "5rem"
                    , style "height" "5rem"
                    ]
                ]
                []

        _ ->
            case model.current of
                Empty ->
                    text ""

                Data newsList ->
                    let
                        viewNews : News -> ListGroup.CustomItem Msg
                        viewNews news =
                            viewNewsInteractive news model.selected
                    in
                    ListGroup.custom <|
                        List.map viewNews newsList

                Error msg ->
                    text msg


viewNewsInteractive : News -> Maybe News -> ListGroup.CustomItem Msg
viewNewsInteractive news selectedNews =
    let
        itemStyle =
            case selectedNews of
                Just value ->
                    if value.id == news.id then
                        ListGroup.warning

                    else
                        ListGroup.secondary

                Nothing ->
                    ListGroup.secondary

        badge =
            if news.accepted then
                Badge.badgeSuccess [] [ text "Accepted" ]

            else
                Badge.badgeDanger [] [ text "Rejected" ]
    in
    ListGroup.button
        [ itemStyle
        , ListGroup.attrs
            [ onClick (Play news)
            , Flex.block
            , Flex.justifyBetween
            , Flex.alignItemsCenter
            ]
        ]
        [ text news.title, badge ]


viewStart : State -> Html Msg
viewStart state =
    if state.start == 0 then
        text ""

    else
        case state.requestor of
            NoOne ->
                Button.button
                    [ Button.roleLink
                    , Button.onClick ToStartPage
                    ]
                    [ text "Start" ]

            Start ->
                Button.button
                    [ Button.roleLink
                    , Button.disabled True
                    ]
                    [ Spinner.spinner
                        [ Spinner.small
                        , Spinner.color Text.warning
                        , Spinner.attrs [ Spacing.mr1 ]
                        ]
                        []
                    , text "Loading..."
                    ]

            _ ->
                Button.button
                    [ Button.roleLink
                    , Button.disabled True
                    ]
                    [ text "Start" ]


viewPrevious : State -> Html Msg
viewPrevious state =
    if state.start == 0 then
        text ""

    else
        case state.requestor of
            NoOne ->
                Button.button
                    [ Button.roleLink
                    , Button.onClick ToPreviousPage
                    ]
                    [ text "Previous" ]

            Previous ->
                Button.button
                    [ Button.roleLink
                    , Button.disabled True
                    ]
                    [ Spinner.spinner
                        [ Spinner.small
                        , Spinner.color Text.warning
                        , Spinner.attrs [ Spacing.mr1 ]
                        ]
                        []
                    , text "Loading..."
                    ]

            _ ->
                Button.button
                    [ Button.roleLink
                    , Button.disabled True
                    ]
                    [ text "Previous" ]


viewNext : State -> Html Msg
viewNext state =
    case state.requestor of
        NoOne ->
            case state.next of
                Data news ->
                    if List.isEmpty news then
                        text ""

                    else
                        Button.button
                            [ Button.roleLink
                            , Button.onClick ToNextPage
                            ]
                            [ text "Next" ]

                _ ->
                    text ""

        Next ->
            Button.button
                [ Button.roleLink
                , Button.disabled True
                ]
                [ Spinner.spinner
                    [ Spinner.small
                    , Spinner.color Text.warning
                    , Spinner.attrs [ Spacing.mr1 ]
                    ]
                    []
                , text "Loading.."
                ]

        Start ->
            Button.button
                [ Button.roleLink
                , Button.disabled True
                ]
                [ Spinner.spinner
                    [ Spinner.small
                    , Spinner.color Text.warning
                    , Spinner.attrs [ Spacing.mr1 ]
                    ]
                    []
                , text "Loading.."
                ]

        _ ->
            Button.button
                [ Button.roleLink
                , Button.disabled True
                ]
                [ text "Next" ]



-- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.batch [ Tab.subscriptions model.tabState TabMsg ]
