{-# LANGUAGE OverloadedStrings #-}

module Config.ConfigDb (withDbConnection) where

import Database.Persist.MySQL (withMySQLConn, defaultConnectInfo, ConnectInfo(..))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (MonadIO)

-- Função para conectar ao banco de dados
withDbConnection :: MonadIO m => (SqlBackend -> m a) -> String -> String -> String -> String -> m a
withDbConnection action connectHost connectUser connectPassword connectDatabase = runStdoutLoggingT $ withMySQLConn connInfo action
  where
    connInfo = defaultConnectInfo {
        connectHost     = connectHost,
        connectUser     = connectUser,
        connectPassword = connectPassword,
        connectDatabase = connectDatabase
    }