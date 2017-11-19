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

import S3 exposing ( readCredentials )

import AWS.Core.Credentials exposing ( Credentials )
import AWS.Core.Service as Service exposing ( Service, ApiVersion, Protocol )
import AWS.Core.Http exposing ( Method(..), Response
                              , responseData, emptyBody
                              , request, send
                              )

import Html exposing ( Html, Attribute
                     , div, text, p, h2, table, tr, th, td, a, br
                     , input, button
                     )
import Html.Attributes exposing ( href, type_, size, value, disabled, style )
import Html.Events exposing ( onClick, onInput )
import Http
import Json.Decode as JD
import Task
import Char
import Debug exposing ( log )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

type alias Model =
    { credentials : Maybe Credentials
    , service : Service
    , display : String
    , bucket : String
    }

type Msg
    = SetBucket String
    | ListBucket
    | ReceiveListBucket (Result Http.Error String)
    | ReceiveCredentials (Result Http.Error (Maybe Credentials))

endpointPrefix : String
endpointPrefix =
    "s3"

apiVersion : ApiVersion
apiVersion =
    "2017-07-10"

protocol : Protocol
protocol =
    Service.restXml

digitalOceanHostBase : String
digitalOceanHostBase =
    "digitaloceanspaces.com"

digitalOceanRegion : String
digitalOceanRegion =
    "nyc3"

makeService : String -> Maybe String -> Service
makeService region hostBase =
    let s3 = Service.defineRegional endpointPrefix apiVersion protocol Service.signV4
        host = case hostBase of
                   Nothing ->
                       Nothing
                   Just base ->
                       Just <| region ++ "." ++ base
    in
        s3 (Service.setHost host) region

init : (Model, Cmd Msg)
init =
    ( { credentials = Nothing
      , service = makeService digitalOceanRegion (Just digitalOceanHostBase)
      , display = "Fetching credentials..."
      , bucket = "etwof"
      }
    , Task.attempt ReceiveCredentials (readCredentials Nothing)
    )

listBucket : Model -> Cmd Msg
listBucket model =
    case model.credentials of
        Just credentials ->
            let req = log "req" <|
                      request GET "/" [] emptyBody JD.string
                host = Service.host model.service
                service = Service.setHost 
                          (Just <| model.bucket ++ "." ++ host)
                          model.service
                task = send service credentials req
            in
                Task.attempt ReceiveListBucket task
        _ ->
            Cmd.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetBucket bucket ->
            ( { model | bucket = bucket }
            , Cmd.none
            )
        ListBucket ->
            ( model
            , listBucket model
            )
        ReceiveListBucket result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok res ->
                    ( { model | display = res }
                    , Cmd.none
                    )
        ReceiveCredentials result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok credentials ->
                    ( { model | credentials = credentials
                      , display = case credentials of
                                      Just _ -> "Credentials received."
                                      Nothing -> "Malformed credentials!"
                      }
                    , Cmd.none
                    )

view : Model -> Html Msg
view model =
    div []
        [ p [] [ text model.display ]
        , p [] [ input [ type_ "text"
                       , value model.bucket
                       , onInput SetBucket
                       ]
                     []
               , text " "
               , button [ onClick ListBucket ]
                   [ text "List Bucket" ]
               ]
        ]
