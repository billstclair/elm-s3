module S3 exposing ( readCredentials, makeCredentials )

import AWS.Core.Credentials exposing ( Credentials
                                     , fromAccessKeys )

import Http
import Task exposing ( Task )

defaultCredentialsUrl : String
defaultCredentialsUrl =
    "credentials.txt"

readCredentials : Maybe String -> Task Http.Error (Maybe Credentials)
readCredentials maybeUrl =
    let url = case maybeUrl of
                  Just u -> u
                  Nothing -> defaultCredentialsUrl
        request = Http.getString url
        getTask = Http.toTask request
    in
        Task.map makeCredentials getTask

makeCredentials : String -> Maybe Credentials
makeCredentials string =
    case String.split "/" <| String.trim string of
        [ accessKeyId, secretAccessKey ] ->
            Just <| fromAccessKeys accessKeyId secretAccessKey
        _ ->
            Nothing
