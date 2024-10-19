{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad (void)

import Models.Product

main :: IO ()
main = runSqlite "apiHaskell.db" $ do
    -- Executa as migrações
    void $ runMigration migrateAll

    void $ insert $ Products "Smartphone" "Samsung" 1999.99