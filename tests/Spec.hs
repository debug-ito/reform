{-# LANGUAGE TypeFamilies #-}
module Main (main,spec) where

import Control.Monad.Reader (Reader)
import Test.Hspec
import qualified Text.Reform as R
import qualified Text.Reform.Generalized as RG

newtype SimpleError = SimpleError { unSimpleError :: String }
                      deriving (Show,Eq,Ord)

newtype SimpleInput = SimpleInput { unSimpleInput :: String }
                      deriving (Show,Eq,Ord)

instance R.FormError SimpleError where
  type ErrorInputType SimpleError = SimpleInput
  commonFormError = SimpleError . show

type SimpleForm = R.Form (Reader [(String,String)]) SimpleInput SimpleError ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = undefined
