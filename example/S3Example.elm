----------------------------------------------------------------------
--
-- S3Example.elm
-- Example of using the S3 library
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module S3Example exposing (..)

import S3 exposing ( readAccounts, makeCredentials )
import S3.Types exposing ( Error(..), Account, Key, KeyList
                         , QueryElement(..)
                         )

import AWS.Core.Service as Service

import Html exposing ( Html, Attribute
                     , div, text, p, h2, table, tr, th, td, a, br
                     , input, button
                     , select, option
                     , textarea
                     )
import Html.Attributes exposing ( href, type_, size, value, disabled, style
                                , selected, cols, rows
                                )
import Html.Events exposing ( onClick, onInput, on, targetValue )
import Http
import Json.Decode as JD
import Task
import Char
import List.Extra as LE
import Task
import Debug exposing ( log )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

type alias Model =
    { display : String
    , accounts : List Account
    , account : Account
    , bucket : String
    , key : String
    , keyList : Maybe KeyList
    , text : String
    }

type Msg
    = SetAccount String
    | ReceiveAccounts (Result Error (List Account))
    | ReceiveGetObject (Result Error String)
    | SetBucket String
    | ListBucket
    | ReceiveListBucket (Result Error KeyList)
    | SetKey String
    | GetObject
    | GetKey String
    | SetText String
    | PutObject
    | ReceivePutObject (Result Error String)

init : (Model, Cmd Msg)
init =
    ( { display = "Fetching accounts..."
      , accounts = []
      , account = defaultAccount
      , bucket = "No bucket"
      , key = ""
      , keyList = Nothing
      , text = ""
      }
    , Task.attempt ReceiveAccounts (readAccounts Nothing)
    )

listBucket : Model -> Cmd Msg
listBucket model =
    let task = S3.listKeys model.account model.bucket [ MaxKeys 100 ]
    in
        Task.attempt ReceiveListBucket task

getObject : Model -> Cmd Msg
getObject model =
    let task = S3.getObject model.account model.bucket model.key
    in
        Task.attempt ReceiveGetObject task

putObject : Model -> Cmd Msg
putObject model =
    let task = S3.putHtmlObject model.account model.bucket model.key model.text
    in
        Task.attempt ReceivePutObject task

defaultAccount : Account
defaultAccount =
    { name = "No account"
    , region = Nothing
    , accessKey = ""
    , secretKey = ""
    , buckets = [ "No bucket" ]
    , serviceGetters = Service.s3Getters
    }

findAccount : Model -> String -> Account
findAccount model name =
    case LE.find (\a -> a.name == name) model.accounts of
        Nothing ->
            defaultAccount
        Just a ->
            a

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetAccount name ->
            let account = findAccount model name
                bucket = case account.buckets of
                             b :: _ ->
                                 b
                             _ ->
                                 "No bucket"
            in
                ( { model
                      | account = account
                      , bucket = bucket
                      , display = "Account: " ++ name
                  }
                , Cmd.none
            )
        SetBucket bucket ->
            ( { model | bucket = bucket }
            , Cmd.none
            )
        ListBucket ->
            ( { model | display = "Getting bucket listing..." }
            , listBucket model
            )
        ReceiveListBucket result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok keys ->
                    ( { model
                          | display = "Bucket listing received."
                          , keyList = Just keys }
                    , Cmd.none
                    )
        SetKey key ->
            ( { model | key = key }
            , Cmd.none
            )
        GetObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )
            else
                ( { model | display = "Fetching " ++ model.key ++ "..." }
                , getObject model
                )
        ReceiveGetObject result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok res ->
                    ( { model
                          | display = "Got " ++ model.key
                          , text = res
                      }
                    , Cmd.none
                    )
        GetKey key ->
            ( { model | key = key }
            , Task.perform (\_ -> GetObject) <| Task.succeed ()
            )
        SetText text ->
            ( { model | text = text }
            , Cmd.none
            )
        PutObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )
            else
                ( { model | display = "Writing " ++ model.key ++ "..." }
                , putObject model
                )
        ReceivePutObject result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok res ->
                    ( { model | display = "Put " ++ model.key }
                    , Cmd.none
                    )
        ReceiveAccounts result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok accounts ->
                    let account = case accounts of
                                      a :: _ ->
                                          a
                                      _ ->
                                          defaultAccount
                    in
                        ( { model
                              | accounts = accounts
                              , account = account
                              , bucket = case account.buckets of
                                             b :: _ ->
                                                 b
                                             _ ->
                                                 "No bucket"
                              , display = "Accounts received."
                          }
                        , Cmd.none
                        )
                        
view : Model -> Html Msg
view model =
    div [ style [("margin-left", "3em")]
        ]
        [ p [] [ text model.display ]
        , p [] [ text "Account: "
               , accountSelector model
               ]
        , p [] [ text "Bucket: "
               , bucketSelector model
               , text " "
               , button [ onClick ListBucket ]
                   [ text "List Bucket" ]
               ]
        , p [] [ text "Key: "
               , input [ type_ "text"
                       , size 40
                       , value model.key
                       , onInput SetKey
                       ]
                     []
               , text " "
               , button [ onClick GetObject ]
                   [ text "Get Object" ]
               , text " "
               , button [ onClick PutObject ]
                   [ text "Put Object" ]
               ]
        , p []
            [ textarea [ cols 80
                       , rows 20
                       , value model.text
                       , onInput SetText
                       ]
                  []
            ]                  
        , p []
            [ showKeys model ]
        ]

accountSelector : Model -> Html Msg
accountSelector model =
    select [ on "change" (JD.map SetAccount targetValue) ]
        (List.map (accountOption model) model.accounts)

accountOption : Model -> Account -> Html Msg
accountOption model account =
    option [ value account.name
           , selected (model.account.name == account.name)
           ]
        [ text account.name ]

bucketSelector : Model -> Html Msg
bucketSelector model =
    select [ on "change" (JD.map SetBucket targetValue) ]
        (List.map (bucketOption model) model.account.buckets)

bucketOption : Model -> String -> Html Msg
bucketOption model bucket =
    option [ value bucket
           , selected (model.bucket == bucket)
           ]
        [ text bucket ]

thText : String -> Html Msg
thText string =
    th [] [ text string ]

tdAlignHtml : String -> Html Msg -> Html Msg
tdAlignHtml alignment html =
    td [ style [ ("padding-left", "1em")
               , ("padding-right", "1em")
               , ("text-align", alignment)
               ]
       ]
    [ html ]

tdAlignText : String -> String -> Html Msg
tdAlignText alignment string =
    tdAlignHtml alignment <| text string

tdHtml : Html Msg -> Html Msg
tdHtml html =
    tdAlignHtml "left" html

tdText : String -> Html Msg
tdText string =
    tdAlignText "left" string

showKeys : Model -> Html Msg
showKeys model =
    case model.keyList of
        Nothing ->
            text ""
        Just keyList ->
            table []
                <| List.concat
                    [ [ tr []
                            [ thText "Key"
                            , thText "Size"
                            , thText "Modified"
                            , thText "Owner"
                            ]
                      ]
                    , keyRows keyList
                    ]

keyRows : KeyList -> List (Html Msg)
keyRows keyList =
    List.map keyRow keyList.keys

link : String -> (String -> Msg) -> Html Msg
link string msg =
    a [ href "#"
      , onClick <| msg string
      ]
    [ text string ]

keyRow : Key -> Html Msg
keyRow key =
    tr []
        [ tdHtml <| link key.key GetKey
        , tdAlignText "right" <| toString key.size
        , tdText key.lastModified
        , tdText key.owner.displayName
        ]
