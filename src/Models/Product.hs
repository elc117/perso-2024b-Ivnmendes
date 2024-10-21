{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Models.Product where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Products
    name String
    brand String
    price Double
    deriving Show Generic
|]

instance ToJSON Products where
    toJSON (Products n b p) =
        object
            [
                "name" .= n,
                "brand" .= b,
                "price" .= p
            ]

instance FromJSON ProductData where
    parseJSON = withObject "ProductData" $ \v -> ProductData
        <$> v .: "name"
        <*> v .: "brand"
        <*> v .: "price"

data ProductData = ProductData
    {
        name      :: String
    ,   brand     :: String
    ,   price     :: Double
    } deriving (Show, Generic)

getName :: ProductData -> String
getName p = name p

getBrand :: ProductData -> String
getBrand p = brand p

getPrice :: ProductData -> Double
getPrice p = price p