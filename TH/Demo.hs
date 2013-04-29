-- Demo.hs
-- T. M. Kelley
-- Dec 20, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Demo where

import VecXForm
import qualified Data.Vector.Unboxed -- as U
import qualified Data.Vector.Unboxed.Base

-- | A simple user-defined type with two fields.
data MyT = MyT { gd :: Double
               , gi :: !Int
               } deriving (Show)

myt = MyT 3.14159 42

-- The next line uses the user defined type "MyT" to generate a new
-- type called "MyTVectromatic". Instead of a Double and an Int, the
-- new type will have a vector of Doubles, and a vector of Ints.
mkUnboxedVector ''MyT

-- The next line will generate an instance of the Show class for the
-- freshly minted MyTVectromatic type.
listFieldsAndTypes ''MyTVectromatic

mytv = MyTVectromatic (Data.Vector.Unboxed.fromList [3.14159,6.28318]) (Data.Vector.Unboxed.fromList [42..53])

-- End of file
