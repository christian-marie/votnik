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
import           Data.Set
import           Network.URI
import           Data.Time
import Text.Blaze.Html(Html)
import           Control.Applicative.Free
import           Control.Exception
import           Control.Monad.Free
import           Control.Monad.Identity
import           Data.Monoid
import           Data.ByteString.Lazy as Lazy
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

-- | The monad for validating forms
type Validation = Identity


type Token = Text
type Group = Text
type Expiry = UTCTime

newtype User = User Text

-- | An interactive automation language
--
-- Is expected to negotiate user identity before evaluation, this identity is
-- provided with 'User'.
class Automation f where
    -- | Can't parametrically produce an a, so explode
    abort
        :: Text
        -> f a

    -- | Request input from user with a digestive-functor form and a template.
    input
        :: Form Text Validation r
        -> (View Html -> Html)
        -> (r -> f a)
        -> f a

    finish
        :: Html -- Should be pandoc or something
        -> f ()

    finish_
        :: f ()
    finish_ = finish "done"


    -- | Ask about the user
    user
        :: (User -> f a)
        -> f a

    -- | Run IO
    locally
        :: IO r
        -> (r -> f a)
        -> f a

    locally_
        :: IO r
        -> f ()
    locally_ f = locally f (\_ -> finish_)

    -- | Save to persistent store
    save
        :: Text -- Key
        -> ByteString
        -> f ()

    -- | Lookup persistent store
    lookup
        :: Text
        -> (Maybe ByteString -> f a)
        -> f a
