module Services.ProductsServices where

import Control.Monad.IO.Class (liftIO)
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad (void)

import Models.Product

getAllProducts :: SqlPersistT IO [Entity Products] 
getAllProducts = selectList [] []