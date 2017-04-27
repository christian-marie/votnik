--
-- Copyright © 2017 Christian Marie
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Botlike.AutomationDSL
(
    Automation(..),
    AutomationT(..),
    abort,
    input,

    Form(..),
    InputLabel(..),
    Document(..),
    Validation(..),

    Html,

    module Text.Digestive,
    module Text.Digestive.Blaze.Html5
)
where

import           Control.Applicative

import Text.Blaze.Html(Html)
import           Control.Applicative.Free
import           Control.Exception
import           Control.Monad.Free
import           Control.Monad.Identity
import           Data.Monoid
import           Data.Aeson
import           Data.String
import           Data.Text                (Text)
import Text.Digestive
import Text.Digestive.Blaze.Html5

import qualified Data.Text                as T
import           Text.Pandoc

markdownToRST =
  writeRST def {writerReferenceLinks = True} .
  handleError . readMarkdown def

data DataPtr shape = DataPtr ()
data PlotOpts a = PlotOpts a-- TODO: Module
data Host = Host


-- | A document is just pandoc under the hood
newtype Document = Document Pandoc
  deriving Monoid

-- | Overloaded strings use markdown
instance IsString Document where
    fromString = either throw Document . readMarkdown def

newtype InputLabel = InputLabel Text
  deriving (IsString, Monoid)

type Automation = Free AutomationT
-- | The monad for validating forms
type Validation = Identity

-- | An interactive automation language
data AutomationT a where
    -- | Can't parametrically produce an a, so explode
    AbortT
        :: String
        -> AutomationT a

    InputT :: Form Text Validation r
           -> (View Html -> Html)
           -> (r -> a)
           -> AutomationT a

deriving instance Functor AutomationT

abort :: String -> Automation a
abort = liftF . AbortT

input :: Form Text Validation a -> (View Html -> Html)-> Automation a
input f v = liftF $ InputT f v id

