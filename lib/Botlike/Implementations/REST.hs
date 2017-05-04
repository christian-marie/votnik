--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Botlike.Implementations.REST
(
    main,
) where

import           Botlike
import           Botlike.Implementations.MVarKernel

import           Control.Applicative.Free           (runAp_)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception                  (throw)
import           Control.Lens                       hiding ((.=))
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except         (ExceptT, throwE)
import           Data.Aeson
import           Data.Aeson.Types                   (Pair)
import qualified Data.ByteString.Lazy.Char8         as BS
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import           Data.String                        (IsString (..))
import           Data.Swagger
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Word                          (Word64)
import           GHC.Generics                       (Generic)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Environment                 (getArgs, lookupEnv)
import           Text.Digestive.Aeson
import           Text.Digestive.View
import           Text.Read                          (readMaybe)


instance ToSchema ClientStep where
  declareNamedSchema _ = return $ NamedSchema (Just "Step") $ mempty

newtype Submission = Submission { unSubmission :: Value }
  deriving (Generic, FromJSON)
instance ToSchema Submission where
  declareNamedSchema _ = return $ NamedSchema (Just "Submission") $ mempty

stepLink :: AutomationID -> StepID -> URI
stepLink = safeLink api path
    where
        path :: Proxy ("step" :> Capture "interaction_id" AutomationID :> Capture "step_id" StepID :> ReqBody '[JSON] Submission :> Post '[JSON] ClientStep)
        path = Proxy

instance ToJSON ClientStep where
    toJSON (RequestInput aid sid (InputDescription f v)) =
        object [ "next_step_id" .= show (stepLink aid sid) ]

    toJSON (TryLater aid sid) =
        object [ "next_step_id" .= show (stepLink aid sid) ]

    toJSON (FinishWith html) =
        object [ "finished" .= ("EOF" :: Text)]

    toJSON (Abort msg) =
        object [ "abort" .= msg ]

type InteractionAPI =
    "step" :> Capture "interaction_id" AutomationID :> Post '[JSON] ClientStep
    :<|> "step" :> Capture "interaction_id" AutomationID :> Capture "step_id" StepID :> ReqBody '[JSON] Submission :> Post '[JSON] ClientStep

type SwaggeredAPI =
    -- this serves both: swagger.json and swagger-ui
    SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> InteractionAPI

-- | Embed APIError as JSON within ServantErr body. TODO: lens for
-- bidirectionality?
jsonErr :: ServantErr -> APIError -> ServantErr
jsonErr err a = err { errBody = encode a, errHeaders = [("Content-Type","application/json")]}

-- | Embedded within ServantErr as more structured JSON thing.
data APIError
    = ValidationError { validations :: Value }
    | StepNotFound
    | InteractionNotFound
    | NotYetReady StepID
  deriving (Generic)
instance FromJSON APIError
instance ToJSON APIError

server :: MVar SimpleDB -> Server SwaggeredAPI
server db =
        jensolegSwaggerSchemaUIServer swaggerDoc
        :<|> firstStep
        :<|> nthStep
      where
        -- | Given a selection of the interaction to begin, begin running until
        -- we generate the first page that requires a reponse, if we require a
        -- response.
        firstStep :: AutomationID -> ExceptT ServantErr IO ClientStep
        firstStep iid = do
            case Map.lookup iid automations of
                Nothing -> throwE $ jsonErr err404 InteractionNotFound
                Just (MVarKernel k) -> liftIO $ k iid db

        -- | Given that we've already started interacting, the user is
        -- submitting a response.
        nthStep :: AutomationID -> StepID -> Submission -> ExceptT ServantErr IO ClientStep
        nthStep iid sid (Submission json) = do
            -- What is the client expecting us to do next?
            s <- getServerStep db sid
            case s of
                Nothing ->
                    throwE $ jsonErr err404 StepNotFound
                Just (Validate (InputDescription form _template) k) ->
                    case runIdentity $ digestJSON form json of
                        -- | Validation passed
                        (v, Just a) ->
                            liftIO $ k a
                        -- | Validation failed
                        (v, Nothing) ->
                            throwE $ jsonErr err404 (ValidationError $ jsonErrors v)
                Just (AsyncIO k) ->
                    liftIO k


swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy InteractionAPI)
    & info.title       .~ "The API"
    & info.version     .~ "0.0.1"
    & info.description ?~ "An API for automation integration"

api :: Proxy SwaggeredAPI
api = Proxy

app :: MVar SimpleDB -> Application
app dbimpl = serve api (server dbimpl)

main :: IO ()
main = do
    m <- newMVar (Map.empty, 0)
    Warp.run 8080 (app m)
