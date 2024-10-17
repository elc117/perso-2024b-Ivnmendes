{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config.ConfigDb (withDbConnection)
import Models.Product (Product(..), migrateAll)
import Database.Persist (insert, get)
import Database.Persist.Sql (runSqlPersistM)
import Control.Monad.IO.Class (liftIO)

import Config.ConfigDb

main :: IO ()
main = do
    -- Variáveis para acesso ao banco
    let dbName = "test"
    let user = "root"
    let password = ""

    -- Criar a string de conexão
    let dbConnector = "host=localhost dbname=" ++ dbName ++ " user=" ++ user ++ " password=" ++ password

    -- Exibir a string de conexão
    putStrLn dbConnector
