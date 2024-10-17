{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeFamilies #-}

module Entities.Product where

import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Product
    name Int
    brand String
    price Float
    deriving Show
|]