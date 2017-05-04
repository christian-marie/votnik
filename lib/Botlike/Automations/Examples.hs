--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Botlike.Automations.Examples
(
    ex1
) where

import           Botlike

import           Control.Applicative
import           Data.Monoid
import           Data.String

import qualified Data.Text                as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Digestive.Blaze.Html5 as H2


ex1 :: Automation f => f ()
ex1 = 
    input form template $  \x -> 
        locally_ (print x)
  where
    form = (,) <$> "name" .: text Nothing
               <*> "mail" .: check "Empty" (not . Text.null) (text Nothing)

    template :: View Html -> Html
    template view = do
        H2.form view "" $ do
            label     "name" view "Name: "
            inputText "name" view
            H.br

            errorList "mail" view
            label     "mail" view "Email address: "
            inputText "mail" view
            H.br
            inputSubmit "Unfurl the monad"
 
