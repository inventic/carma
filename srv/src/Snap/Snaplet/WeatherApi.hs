{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.WeatherApi ( Weather
                               , WeatherApiHandler
                               , WApiHandler
                               , weatherApiInit
                               ) where

import Snap

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.UTF8 (toString, fromString)

import WeatherApi
import WeatherApi.Google

import Control.Monad.IO.Class(liftIO)
import Data.Maybe

import Data.Aeson

data WApiHandler = WApiHandler { _h :: WeatherApiHandler }
makeLens ''WApiHandler

instance ToJSON Weather where
   toJSON (Weather _ temp _ _ _) =
       object ["tempC" .= temp]

retrieveWeather :: Handler b WApiHandler ()
retrieveWeather =  do
    modifyResponse $ setContentType "application/json"
    city    <- getParam "city"
    h       <- gets _h
    weather <- liftIO $
      getWeather h $ toString $ fromMaybe "" city
    response weather
    return ()

response (Left (NotFoundError _)) = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeBS $ fromString "город не найден"

response (Left (NetworkError  a)) = do
  modifyResponse $ setResponseStatus 500 "Internal Server Error"
  writeBS $ fromString $ "неудалось получить: " ++ a

response (Right a) =
    writeLBS $ encode a

routes :: [(B.ByteString, Handler b WApiHandler ())]
routes = [ ("/:city", method GET $ retrieveWeather) ]

weatherApiInit :: SnapletInit b WApiHandler
weatherApiInit =
    makeSnaplet "weatherapi" "get weather by city." Nothing $ do
      addRoutes routes
      return $ WApiHandler $ mkWeatherHandler $ initApi "ru" "utf-8"
