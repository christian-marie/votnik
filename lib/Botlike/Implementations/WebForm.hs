--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE TypeSynonymInstances              #-}
{-# LANGUAGE Rank2Types              #-}

module Botlike.Implementations.WebForm
(
) where

import Botlike.Implementations.REST
import Data.Text(Text)
import qualified Data.Text.Encoding as T
import Botlike.AutomationDSL
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Snap.Http.Server
import Snap.Core  hiding (method)
import           Data.Map (Map)
import           qualified Data.Map as Map
import           Snap.Blaze
import Text.Digestive.Snap
import qualified Text.Digestive.Form.Internal as D
import qualified Text.Blaze.Html5 as H





site :: DBImpl Snap -> Snap ()
site dbimpl = route [ ("step/:interaction_id/:step_id", stepHandler dbimpl)]

writeError :: Int -> LBS.ByteString -> Snap a
writeError code lbs = do
    modifyResponse $ setResponseCode code
    writeLBS lbs
    getResponse >>= finishWith

stepHandler :: DBImpl Snap -> Snap ()
stepHandler DBImpl{..} = do
    i <- getParam "interaction_id" >>= (\x -> case x of
        Just bs -> case BS.readInteger bs of
                        Nothing -> writeError 400 "Invalid interaction_id"
                        Just (iid, _rem) -> getInteraction $ InteractionID (fromIntegral iid)
        Nothing -> writeError 400 "Must specify 'interaction_id'")

    m_cont <- getParam "step_id" >>= (\x -> case x of
        Just bs -> case BS.readInteger bs of
                        Nothing -> writeError 400 "Invalid step_id"
                        Just (sid, _rem) -> getCont $ StepID (fromIntegral sid)
        Nothing -> return Nothing)

    case m_cont of
        Nothing ->
            go i
        Just (UserCont form template k) -> do
            (view, m_a) <- runForm "form" (hoistForm nat form)
            case m_a of
                Nothing -> renderForm template view
                Just a -> go (k a)
  where
    nat :: Validation a -> Snap a
    nat = return . runIdentity

    renderForm template view = blaze . template . fmap H.toHtml $ view


    go :: Automation () -> Snap ()
    go (Pure ()) = return ()
    go (Free (InputT form template k)) = do
        sid <- saveCont (UserCont form template k)
        let v = runIdentity $ getForm "form" form
        renderForm template v

getInteraction iid =
    case Map.lookup iid interactions of 
        Nothing -> writeError 404 "Interaction not found"
        Just x -> return x

main :: IO ()
main = do
    m <- newMVar (Map.empty, 0)
    quickHttpServe (site (mvarDBImpl m))



-- We need forms to be functors over monads
hoistForm :: Monad g => (forall a. f a -> g a) -> Form v f a -> Form v g a
hoistForm f (D.Ref r x) = D.Ref r (hoistForm f x)
hoistForm f (D.Pure x) = D.Pure x
hoistForm f (D.App x y) = D.App (hoistForm f x) (hoistForm f y)
hoistForm f (D.Map g x) = D.Map (\b -> f $ g b) (hoistForm f x)
hoistForm f (D.Monadic x) = D.Monadic $ f x >>= return . hoistForm f

