{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Products
    name String
    brand String
    price Double
    deriving Show
|]

data ProductData = ProductData
    {
        name      :: String
    ,   brand     :: String
    ,   price     :: Double
    } deriving (Show)

getName :: ProductData -> String
getName p = name p

getBrand :: ProductData -> String
getBrand p = brand p

getPrice :: ProductData -> Double
getPrice p = price p