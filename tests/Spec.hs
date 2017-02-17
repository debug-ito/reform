{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Main (main,spec) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (Reader, runReader, ask)
import Data.List (lookup)
import Data.Text.Lazy (Text)
import Test.Hspec
import qualified Text.Reform as R
import qualified Text.Reform.Generalized as RG

newtype SimpleError = SimpleError String
                      deriving (Show,Eq,Ord)

newtype SimpleInput = SimpleInput String
                      deriving (Show,Eq,Ord)

instance R.FormError SimpleError where
  type ErrorInputType SimpleError = SimpleInput
  commonFormError = SimpleError . show

type SimpleForm = R.Form (Reader [(String,String)]) SimpleInput SimpleError String ()

inputRead :: (Read a, Show a) => a -> SimpleForm a
inputRead init_val = RG.input parse render init_val where
  parse (SimpleInput i) = case filter (\(_, leftover) -> null leftover) $ reads i of
    [] -> Left $ SimpleError $ ("parse error: " ++ i)
    ((ret, _) : _) -> Right ret
  render fid val = "{" ++ show fid ++ ": " ++ show val ++ "}"

formIdStr :: String -> Int -> String
formIdStr prefix num = prefix ++ "-fval[" ++ show num ++ "]"

expectedFormView :: String -> Int -> String -> String
expectedFormView prefix form_id_num val_str = "{" ++ formIdStr prefix form_id_num ++ ": " ++ val_str ++ "}"

readerEnv :: R.Environment (Reader [(String,String)]) SimpleInput
readerEnv = R.Environment $ \fid -> do
  pairs <- ask
  case lookup (show fid) pairs of
   Nothing -> return $ R.Missing
   Just v -> return $ R.Found $ SimpleInput v

formToView :: Text -> SimpleForm a -> String
formToView prefix form = flip runReader [] $ R.viewForm prefix form

formToData :: Text -> [(String,String)] -> SimpleForm a -> Either String a
formToData prefix env form = flip runReader env $ R.eitherForm readerEnv prefix form

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Form" $ do
  spec_ap

spec_ap :: Spec
spec_ap = describe "Applicative construction" $ do
  it "should construct a flat struct with increasing FormId" $ do
    let form :: SimpleForm (Int,Int,Int)
        form = (,,) <$> inputRead 1 <*> inputRead 2 <*> inputRead 3
        got = formToView "hoge" form
    got `shouldBe` ( expectedFormView "hoge" 0 "1"
                     ++ expectedFormView "hoge" 1 "2"
                     ++ expectedFormView "hoge" 2 "3"
                   )
    let env = [ (formIdStr "hoge" 0, "100"),
                (formIdStr "hoge" 1, "200"),
                (formIdStr "hoge" 2, "300")
              ]
        got_data = formToData "hoge" env form
    got_data `shouldBe` Right (100,200,300)
    
  it "should construct a nested struct with increasing FormId" $ do
    let form :: SimpleForm (Int, (Int, (Int, Int), Int), Int, (Int, Int))
        form = (,,,) <$> inputRead 1 <*> form_sub1 <*> inputRead 2 <*> form_sub2
        form_sub1 = (,,) <$> inputRead 11 <*> form_ssub <*> inputRead 12
        form_ssub = (,) <$> inputRead 21 <*> inputRead 22
        form_sub2 = (,) <$> inputRead 13 <*> inputRead 14
        got = formToView "foo" form
    got `shouldBe` ( expectedFormView "foo" 0 "1"
                     ++ expectedFormView "foo" 1 "11"
                     ++ expectedFormView "foo" 2 "21"
                     ++ expectedFormView "foo" 3 "22"
                     ++ expectedFormView "foo" 4 "12"
                     ++ expectedFormView "foo" 5 "2"
                     ++ expectedFormView "foo" 6 "13"
                     ++ expectedFormView "foo" 7 "14"
                   )
    let env = map (\fid -> (formIdStr "foo" fid, show ((fid + 1) * 100))) [0 .. 10]
        got_data = formToData "foo" env form
    got_data `shouldBe` Right (100, (200, (300, 400), 500), 600, (700, 800))
