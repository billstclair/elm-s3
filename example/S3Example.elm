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
import AWS.Core.Service exposing ( Service, defineGlobal )
import AWS.Core.Http exposing ( Method(..), Response
                              , responseData, emptyBody
                              )

import Html exposing ( Html, Attribute
                     , div, text, p, h2, table, tr, th, td, a, br
                     , input, button
                     )
import Html.Attributes exposing ( href, type_, size, value, disabled, style )
import Html.Events exposing ( onClick, onInput )
import Http
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
    , service : Maybe Service
    , display : String
    }

type Msg
    = ReceiveCredentials (Result Http.Error (Maybe Credentials))

init : (Model, Cmd Msg)
init =
    ( { credentials = Nothing
      , service = Nothing
      , display = "Fetching credentials..."
      }
    , Task.attempt ReceiveCredentials (readCredentials Nothing)
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveCredentials result ->
            case result of
                Err err ->
                    ({ model | display = toString err }
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
        ]
