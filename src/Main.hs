{-# LANGUAGE OverloadedStrings #-}

import Database.Persist()
import Database.Persist.Sqlite
import Control.Monad (void)
import qualified Web.Scotty as Scotty       -- Necessário pois o método get do scotty entra em conflito com get do persistent

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)

import Models.Product
import Routes.Routes(routes)                --confuso

initializeDb :: IO ()
initializeDb = runSqlite "apiHaskell.db" $ do
    -- Executa as migrações
    void $ runMigration migrateAll

main :: IO ()
main = do
    initializeDb
    putStrLn "Servidor iniciado em http://localhost:3000"
    Scotty.scotty 3000 $ do
        Scotty.middleware logStdoutDev
        Scotty.middleware $ staticPolicy (addBase "static")
        routes