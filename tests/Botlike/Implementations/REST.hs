--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Botlike.Implementations.REST

import Data.Aeson
import Data.Aeson.Diff
import Data.Aeson.QQ
import Test.Hspec

import Botlike.Implementations.Rest

spec :: Spec
spec =
    describe "/step/:interaction" $ do
        it "404s on nonexistent interactions" $
            let expected = [aesonQQ| { "error" : "no" } |]
            in compareJSON expected expected

  where
    compareJSON a b = diff (toJSON a) b `shouldBe` Patch []

