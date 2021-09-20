{-# LANGUAGE ExtendedDefaultRules #-}

module YouTube where

import           Data.Aeson
import           Data.Text
import           Network.HTTP.Req
import           Protolude               hiding ( Option )

default (Text)

fetch token query = req
    GET
    (https "youtube.googleapis.com/youtube/v3/search" /: query)
    NoReqBody
    jsonResponse
    (options token query)

options token query =
    oAuth2Bearer (encodeUtf8 token)
        <> ("key" =: token)
        <> ("part" =: "snippet")
        <> ("q" =: query)
        <> ("maxResults" =: "1")
