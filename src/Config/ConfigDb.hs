{-# LANGUAGE OverloadedStrings #-}

module Config.ConfigDb where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.MySQL
import Database.Persist.Sql (SqlBackend)

mySqlConnString :: ConnectInfo
mySqlConnString = defaultConnectInfo
    { connectHost     = "localhost"
    , connectPort     = 3306
    , connectUser     = "ivnm"
    , connectPassword = "sua_senha"
    , connectDatabase = "seu_banco_de_dados"
    }

-- Função para obter o pool de conexões
createMySQLPool :: IO (ConnectionPool)
createMySQLPool = runStderrLoggingT $ createMySQLPool mySqlConnString 10