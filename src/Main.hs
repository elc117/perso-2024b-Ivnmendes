{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad (void)

import Models.Product
import Services.ProductsServices(getAllProducts)

main :: IO ()
main = do
    runSqlite "apiHaskell.db" $ do
        -- Executa as migrações
        void $ runMigration migrateAll

        products <- getAllProducts

        liftIO $ print (products :: [Entity Products])