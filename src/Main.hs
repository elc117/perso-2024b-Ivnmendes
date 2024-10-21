{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Database.Persist()
import Database.Persist.Sqlite
import Control.Monad (void)

import Models.Product
import Services.ProductsServices

main :: IO ()
main = do
    runSqlite "apiHaskell.db" $ do
        -- Executa as migrações
        void $ runMigration migrateAll

    -- Pega todos os produtos
        products <- getAllProducts
        liftIO $ print (products :: [(Key Products, Products)])
    -- Pega um produto pelo id
        -- products <- getProductById 2
        -- liftIO $ print (products :: Maybe Products)
    -- Cria um novo produto
        -- let newProduct = ProductData "Televisão" "Sony" 12000.00
        -- products <- createProduct newProduct
        -- liftIO $ print products
    -- Deleta um produto
        -- deleted <- deleteProduct 3
        -- if deleted
            -- then liftIO $ putStrLn "Produto deletado com sucesso."
            -- else liftIO $ putStrLn "Nenhum produto foi encontrado para deletar."
    -- Atualiza o campo name de um produto
        -- products <- updateProductName 2 "Computador"
        -- liftIO $ print products
    -- Atualiza o campo brand de um produto
        -- products <- updateProductBrand 2 "Dell"
        -- liftIO $ print products
    -- Atualiza o campo price de um produto
        -- products <- updateProductPrice 2 3000.00
        -- liftIO $ print products