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
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | A MVar of Map of continuations based implementation, state is considered
-- memory-ephemeral.
--
-- Pro:
--
--  * Simple
--  * Fast
--  * Avoids serialisation
--  * Reuseable by simple backed implementations
--
-- Con:
--
--  * Requires sticky sessions for load balancing over HTTP
--  * State is blown away on restart

module Botlike.Implementations.MVarKernel
(
    MVarKernel(..),
    ServerStep(..),
    ClientStep(..),
    StepID(..),
    AutomationID(..),
    InputDescription(..),
    SimpleDB,
    getServerStep,
    automations
) where

import           Botlike
import           Botlike.Automations.Examples

import           Control.Applicative.Free     (runAp_)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception            (throw)
import           Control.Lens                 hiding ((.=))
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT, throwE)
import           Data.Aeson
import           Data.Aeson.Types             (Pair)
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import qualified Network.Wai.Handler.Warp     as Warp
import           System.Environment           (getArgs, lookupEnv)
import           Text.Digestive.Aeson
import           Text.Digestive.View
import           Text.Read                    (readMaybe)

newtype AutomationID = AutomationID { unAutomationID :: Word64 }
  deriving (Eq, Ord, Enum, Num, Generic)

newtype StepID = StepID { unStepID :: Word64 }
  deriving (Generic, Enum, Eq, Ord, Num, FromJSON, ToJSON )

-- | The type that the user interacts with is simply a function from the
-- selected InteractionID,
data MVarKernel a = MVarKernel (AutomationID -> MVar SimpleDB -> IO ClientStep)
type SimpleDB = (Map StepID ServerStep, StepID)

-- | A step for the client consists of data to display and, optionally, a
-- pointer for submitting information to continue
data ClientStep
    -- | Ask for a form to be filled
    = forall a. RequestInput AutomationID StepID (InputDescription a)
    -- | Check back in for e.g. async IO
    | TryLater AutomationID StepID
    -- | Display a final form, no more to do.
    | FinishWith Html
    -- | Display reason for explosion
    | Abort Text

-- | A form viewy thing
data InputDescription a = InputDescription (Form Text Validation a) (View Html -> Html)

-- | What to do next, when the client returns.
data ServerStep
    -- | Wait for valid input before applying the continuation
    = forall a. Validate (InputDescription a) (a -> IO ClientStep)
    -- | Wait for Async to terminate
    | AsyncIO (IO ClientStep)

saveServerStep mvar k = do
    liftIO $ modifyMVar mvar $ \(m, ptr) ->
        return $ ((Map.insert ptr k m, succ ptr), ptr)

getServerStep mvar sid = do
    liftIO $ withMVar mvar $ \(m, _ptr) -> return $ Map.lookup sid m

automations :: Automation f => Map AutomationID (f ())
automations = Map.fromList $ zip [0..] is
  where
    is = [ ex1 ]

instance Automation MVarKernel where
    input form template k =
        let desc = InputDescription form template
        in MVarKernel $ \aid db -> do
            sid <- mfix $ \sid ->
                saveServerStep db $ Validate desc $ \a ->
                    case k a of MVarKernel i -> i aid db
            return $ RequestInput aid sid desc

    abort str = throw $ userError $ Text.unpack str

    finish html = MVarKernel $ \aid db -> do
        -- XXX Delete step
        return $ FinishWith html

    locally m k = MVarKernel $ \aid db -> do
            todo <- liftIO $ async m
            sid <- mfix $ \sid -> saveServerStep db $ AsyncIO $ do
                r <- liftIO $ poll todo
                case r of
                    Nothing        -> return $ TryLater aid sid
                    Just (Left e)  -> return . Abort . Text.pack $ show e
                    Just (Right a) -> let MVarKernel i = k a in i aid db
            return $ TryLater aid sid
