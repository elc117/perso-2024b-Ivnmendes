{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config.ConfigDb
import Models.Product

import Database.Persist (insert, get)
import Database.Persist.Sql (runSqlPersistM)
import Database.Persist.MySQL (runMySQLPool)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    -- Criar o pool de conexões
    pool <- createMySQLPool

    -- Executar migrações
    runSqlPool (runMigration migrateAll) pool