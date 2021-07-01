{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.Vector as Vector
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.Protocol.XmlParsing as XmlParsing
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Text.XML as Xml
import qualified XmlParser as Xp
import Prelude

main =
  defaultMain $
    let Right response =
          unsafePerformIO $
            Xp.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
     in testGroup
          "Subscribe Response"
          [ testCase "Top level properties" $ do
              assertEqual "" (Just "Handle1") (#serverSubHandle response),
            testCase "DateTime" $ do
              assertEqual "" (Just (read "2019-09-23 16:01:50.576+00:00")) (fmap #rcvTime (#subscribeResult response)),
            testCase "Item value at offset 0" $ do
              assertEqual
                ""
                (Just (FloatValue 4.5))
                ((join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 0) . fmap #items . #rItemList) response),
            testCase "Item value at offset 1" $ do
              assertEqual
                ""
                (Just (IntValue 1234))
                ((join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 1) . fmap #items . #rItemList) response),
            testCase "Item value at offset 2" $ do
              assertEqual
                ""
                (Just (ArrayOfUnsignedShortValue [0, 0, 3, 11, 0, 0]))
                ((join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 2) . fmap #items . #rItemList) response)
          ]
