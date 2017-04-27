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
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Botlike.Implementations.REST
(
    main,
    InteractionID(..),
    StepID(..),
    Step(..),

    interactions,
    mvarDBImpl,
    DBImpl(..),
    UserCont(..)
    
) where

import           Botlike
import           Botlike.Automations.Examples

import           Control.Applicative.Free     (runAp_)
import           Control.Concurrent.MVar
import           Control.Exception            (throw)
import           Control.Lens                 hiding ((.=))
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Except   (ExceptT, throwE)
import           Control.Monad.IO.Class
import           Data.Aeson
import           qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Aeson.Types             (Pair)
import           Data.Word(Word64)
import           Data.Map (Map)
import           qualified Data.Map as Map
import           Data.Maybe                   (fromMaybe)
import           Data.String                  (IsString (..))
import           Data.Swagger
import           Data.Text                    (Text)
import           Text.Digestive.View
import Text.Digestive.Aeson
import           GHC.Generics                 (Generic)
import qualified Network.Wai.Handler.Warp     as Warp
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Environment           (getArgs, lookupEnv)
import           Text.Read                    (readMaybe)


-- | Page to display and maybe a place to submit your response.
data Step = forall v. Step (View v) InteractionID (Maybe StepID)

instance ToSchema Step where
  declareNamedSchema _ = return $ NamedSchema (Just "Step") $ mempty

newtype InteractionID = InteractionID { unInteractionID :: Word64 }
  deriving (Eq, Ord, Enum, Num, Generic, FromHttpApiData, ToHttpApiData)
instance ToParamSchema InteractionID

newtype Submission = Submission { unSubmission :: Value }
  deriving (Generic, FromJSON)
instance ToSchema Submission where
  declareNamedSchema _ = return $ NamedSchema (Just "Submission") $ mempty

newtype StepID = StepID { unStepID :: Word64 }
  deriving (Generic, FromHttpApiData, Enum, Eq, Ord, Num, ToJSON, ToHttpApiData)
instance ToParamSchema StepID


{--

step = foldFree f
  where
    f :: AutomationT ~> Interaction
    -- Render this page, leave the rest of the unevaluated free monad in a
    -- server-side continuation awaiting the answers. Both share a unique
    -- identifier.
    f (PageT form k) = undefined
    f (AbortT err) = undefined
    --}


{--
-- | Make a http form parser from a Page
formParser :: Page a -> Form -> Either Text a
formParser page form = runPage f page
  where
    f :: Page ~> Either Text
    f (InputText (InputLabel l)) = parseUnique l
--}

instance ToJSON Step where
    toJSON (Step view iid m_sid) = do
        let step_path = Proxy :: Proxy ("step" :> Capture "interaction_id" InteractionID :> Capture "step_id" StepID :> ReqBody '[JSON] Submission :> Post '[JSON] Step)
        let g = case m_sid of Nothing -> id
                              Just sid -> ("next_step_id" .= show (safeLink api step_path iid sid) :)
        object . g $ []
{--
instance ToJSON Step where
     toJSON (Step p k_id) =
        let g = case k_id of Nothing -> id
                             Just (StepID uuid) -> ("next_step_id" .= toText uuid :)
        in object . g $ runAp_ f p
      where
        f :: Field a -> [Pair]
        f (InputText label) = [ "type" .= ("text" :: Text), "label" .= ("lbl" :: Text)]
        f (InputHost label) = [ "type" .= ("host" :: Text), "label" .= ("lbl" :: Text)]
        f (InputBool label) = [ "type" .= ("bool" :: Text), "label" .= ("lbl" :: Text)]
        f (Block label) = [ "type" .= ("block" :: Text), "label" .= ("lbl" :: Text)]

--}
type InteractionAPI =
    "step" :> Capture "interaction_id" InteractionID :> Post '[JSON] Step
    :<|> "step" :> Capture "interaction_id" InteractionID :> Capture "step_id" StepID :> ReqBody '[JSON] Submission :> Post '[JSON] Step

type SwaggeredAPI =
    -- this serves both: swagger.json and swagger-ui
    SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> InteractionAPI

interactions :: Map InteractionID (Automation ())
interactions = Map.fromList $ zip [0..] is
  where
    is = [ ex1 ]

data UserCont =
    forall a. UserCont (Form Text Validation a)
                        (View Html -> Html)
                        (a -> Automation ())

data DBImpl m = DBImpl {
    -- | Store the users current continuation so we know where we left off
    -- when we get a response, along with the expected schema.
    saveCont :: UserCont -> m StepID
  , getCont :: StepID -> m (Maybe UserCont)
}

-- | Embed APIError as JSON within ServantErr body. TODO: lens for
-- bidirectionality?
jsonErr :: ServantErr -> APIError -> ServantErr
jsonErr err a = err { errBody = encode a, errHeaders = [("Content-Type","application/json")]}

-- | Embedded within ServantErr as more structured JSON thing.
data APIError
    = ValidationError { validations :: Value }
    | StepNotFound
    | InteractionNotFound
  deriving (Generic)
instance FromJSON APIError
instance ToJSON APIError
  


server :: DBImpl (ExceptT ServantErr IO) -> Server SwaggeredAPI
server DBImpl{..} =
        jensolegSwaggerSchemaUIServer swaggerDoc
        :<|> firstStep
        :<|> nthStep
      where
        -- | Given a selection of the interaction to begin, begin running until
        -- we generate the first page that requires a reponse, if we require a
        -- response.
        firstStep :: InteractionID -> ExceptT ServantErr IO Step
        firstStep iid = do
            i <- getInteraction iid
            stepStep iid i

        -- | Given that we've already started interacting, the user is
        -- submitting a response.
        nthStep :: InteractionID -> StepID -> Submission -> ExceptT ServantErr IO Step
        nthStep iid sid (Submission json) = do
            m_cont <- getCont sid
            UserCont form _template k <- case m_cont of
                Nothing ->
                    throwE $ jsonErr err404 StepNotFound
                Just k -> return k

            case runIdentity $ digestJSON form json of
                -- | Validation passed
                (v, Just a) -> do
                    stepStep iid (k a)
                -- | Validation failed
                (v, Nothing) -> do
                    throwE $ jsonErr err404 (ValidationError $ jsonErrors v)

        getInteraction :: Monad m => InteractionID -> ExceptT ServantErr m (Automation ())
        getInteraction iid = do
            case Map.lookup iid interactions of
                Nothing -> throwE $ jsonErr err404 InteractionNotFound
                Just i -> return i

        stepStep :: InteractionID -> Automation () -> ExceptT ServantErr IO Step
        stepStep iid (Pure a) = return undefined
        stepStep iid (Free (InputT form template k)) = do
            sid <- saveCont $ UserCont form template k
            let v = runIdentity $ getForm "test form" form
            return (Step v iid (Just sid))

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy InteractionAPI)
    & info.title       .~ "The API"
    & info.version     .~ "0.0.1"
    & info.description ?~ "An API for automation integration"

api :: Proxy SwaggeredAPI
api = Proxy

mvarDBImpl :: MonadIO m => MVar (Map StepID UserCont, StepID) -> DBImpl m
mvarDBImpl mvar =
    DBImpl save get
  where
    save k = do
        liftIO $ modifyMVar mvar $ \(m, ptr) ->
            return $ ((Map.insert ptr k m, succ ptr), ptr)
    get sid = do
        liftIO $ withMVar mvar $ \(m, _ptr) -> return $ Map.lookup sid m

app :: DBImpl (ExceptT ServantErr IO) -> Application
app dbimpl = serve api (server dbimpl)

main :: IO ()
main = do
    m <- newMVar (Map.empty, 0)
    Warp.run 8080 (app (mvarDBImpl m))
