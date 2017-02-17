{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Main (main,spec) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (Reader, runReader, ask)
import Data.List (lookup, intercalate)
import Data.Text.Lazy (Text)
import Test.Hspec
import qualified Text.Reform as R
import Text.Reform ((++>), (<++))
import qualified Text.Reform.Generalized as RG

newtype SimpleError = SimpleError { unSimpleError :: String }
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

simpleErrors :: SimpleForm ()
simpleErrors = RG.errors render where
  render [] = "No error"
  render es = intercalate "," $ map unSimpleError es

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
  spec_view

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

spec_view :: Spec
spec_view = describe "view-only construction ((++>) etc.)" $ do
  it "should append together in the Monoid way" $ do
    let form :: SimpleForm ()
        form = R.view "foo" ++> R.view "bar" <++ R.view "buzz"
        got = formToView "hoge" form
    got `shouldBe` "foobarbuzz"

  it "should keep FormId" $ do
    let form :: SimpleForm (Int, (Int, Int), Int)
        form = (,,)
               <$> (R.view "A:" ++> inputRead 10 <++ R.view ", ")
               <*> (R.view "B:" ++> form_sub <++ R.view ", ")
               <*> (R.view "C:" ++> inputRead 20)
        form_sub = (,)
                   <$> (R.view "Ba:" ++> inputRead 110 <++ R.view ", ")
                   <*> (R.view "Bb:" ++> inputRead 120)
        got = formToView "hoge" form
    got `shouldBe` ( "A:" ++ expectedFormView "hoge" 0 "10" ++ ", "
                     ++ "B:"
                     ++ "Ba:" ++ expectedFormView "hoge" 1 "110" ++ ", "
                     ++ "Bb:" ++ expectedFormView "hoge" 2 "120" ++ ", "
                     ++ "C:" ++ expectedFormView "hoge" 3 "20"
                   )

  it "can be used to report errors" $ do
    let form :: SimpleForm (Int,Int,Int)
        form = (,,)
               <$> (inputRead 10 <++ simpleErrors <++ R.view ", ")
               <*> (inputRead 20 <++ simpleErrors <++ R.view ", ")
               <*> (inputRead 30 <++ simpleErrors)
        env = [ (formIdStr "hoge" 0, "100"),
                (formIdStr "hoge" 1, "non-number"),
                (formIdStr "hoge" 2, "300")
              ]
        got_data = formToData "hoge" env form
    got_data `shouldBe` Left ( expectedFormView "hoge" 0 "100" ++ "No error, "
                               ++ expectedFormView "hoge" 1 "20" ++ "parse error: non-number, "
                               ++ expectedFormView "hoge" 2 "300" ++ "No error"
                             )
