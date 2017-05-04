--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Botlike.Implementations.WebForm
(
) where

import           Botlike.AutomationDSL
import           Botlike.Implementations.MVarKernel
import           Control.Concurrent.MVar
import           Control.Monad.Free
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8              as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T
import           Snap.Blaze
import           Snap.Core                          hiding (method)
import           Snap.Http.Server
import qualified Text.Blaze.Html5                   as H
import qualified Text.Digestive.Form.Internal       as D
import           Text.Digestive.Snap

site :: MVar SimpleDB -> Snap ()
site db = route [ ("step/:automation_id/:step_id", stepHandler db)
                , ("step/:automation_id", stepHandler db)
                ]

writeError :: Int -> LBS.ByteString -> Snap a
writeError code lbs = do
    modifyResponse $ setResponseCode code
    writeLBS lbs
    getResponse >>= finishWith

stepHandler :: MVar SimpleDB -> Snap ()
stepHandler db = do
    aid_param <- getParam "automation_id"
    aid <- case aid_param of
        Just bs ->
            case BS.readInteger bs of
                Nothing ->
                    writeError 400 "Invalid automation_id"
                Just (aid, _rem) ->
                    return . AutomationID $ fromIntegral aid
        Nothing ->
            writeError 400 "Must specify 'automation_id'"

    sid_param <- getParam "step_id"

    case sid_param of
        Just bs ->
            case BS.readInteger bs of
                Nothing ->
                    writeError 400 "Invalid step_id"
                Just (sid, _rem) -> do
                    s <- getServerStep db . StepID $ fromIntegral sid
                    case s of
                        Nothing ->
                            writeError 404 "No such step_id"
                        Just (Validate (InputDescription form template) k) -> do
                            (view, m_a) <- runForm "form" (hoistForm nat form)
                            case m_a of
                                -- Not valid input, render form
                                Nothing -> renderForm template view
                                -- Valid input, we have an a
                                Just a  -> liftIO (k a) >>= render
                        Just (AsyncIO k) -> liftIO k >>= render
        -- Request for a new run
        Nothing ->
            case Map.lookup aid automations of
                Nothing ->
                    writeError 404 "No such automation_id"
                Just (MVarKernel k) ->
                    liftIO (k aid db) >>= render
  where
    render :: ClientStep -> Snap ()
    render (RequestInput aid sid (InputDescription form template)) = do
        getForm "form" (hoistForm nat form) >>= renderForm template
    render (TryLater aid sid) = writeError 202 "Processing..."
    render (Abort msg) = writeError 500 (LBS.fromStrict $ T.encodeUtf8 msg)
    render (FinishWith html) = blaze html

    nat :: Validation a -> Snap a
    nat = return . runIdentity

    renderForm template view = blaze . template . fmap H.toHtml $ view

getInteraction aid =
    case Map.lookup aid automations of
        Nothing -> writeError 404 "Interaction not found"
        Just x  -> return x

main :: IO ()
main = do
    m <- newMVar (Map.empty, 0)
    quickHttpServe (site m)

-- We need forms to be functors over monads to use snap library integration
hoistForm :: Monad g => (forall a. f a -> g a) -> Form v f a -> Form v g a
hoistForm f (D.Ref r x)   = D.Ref r (hoistForm f x)
hoistForm f (D.Pure x)    = D.Pure x
hoistForm f (D.App x y)   = D.App (hoistForm f x) (hoistForm f y)
hoistForm f (D.Map g x)   = D.Map (\b -> f $ g b) (hoistForm f x)
hoistForm f (D.Monadic x) = D.Monadic $ f x >>= return . hoistForm f

