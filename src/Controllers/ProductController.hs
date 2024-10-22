{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.ProductController where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Database.Persist.Sqlite (runSqlite)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status
import Web.Scotty (json, ActionM, pathParam, status, jsonData)

import qualified Services.ProductsServices as ProductServices
import Models.Product

data UpdateProductNameData = UpdateProductNameData
  { name :: String
  } deriving (Show, Generic)

instance FromJSON UpdateProductNameData

data UpdateProductBrandData = UpdateProductBrandData
  { brand :: String
  } deriving (Show, Generic)

instance FromJSON UpdateProductBrandData

data UpdateProductPriceData = UpdateProductPriceData
  { price :: Double
  } deriving (Show, Generic)

instance FromJSON UpdateProductPriceData

getAllProducts :: ActionM ()
getAllProducts = do
    products <- liftIO $ runSqlite "apiHaskell.db" ProductServices.getAllProducts  -- Provavelmente não é o modo mais perfomático, mas foi a maneira que eu achei
    status status200
    json $ object ["products" .= products]

getProductById :: ActionM ()
getProductById = do
    productId <- pathParam "id"
    products <- liftIO $ runSqlite "apiHaskell.db" $ ProductServices.getProductById productId
    case products of
        Just _ -> do                      
            status status200
            json $ object ["products" .= products]
        Nothing -> do
            status status404 
            json $ object ["message" .= ("Nenhum produto encontrado com esse id" :: String)] 
    
createProduct :: ActionM ()
createProduct = do
    p <- jsonData :: ActionM ProductData
    products <- liftIO $ runSqlite "apiHaskell.db" $ ProductServices.createProduct p
    case products of
        Just _ -> do                      
            status status201
            json $ object ["products" .= products]
        Nothing -> do
            status status400 
            json $ object ["message" .= ("Um ou mais campos inválidos" :: String)]

deleteProduct :: ActionM ()
deleteProduct = do
    productId <- pathParam "id"
    deletedRows <- liftIO $ runSqlite "apiHaskell.db" $ ProductServices.deleteProduct productId
    if deletedRows
        then
            status status204
        else do
            status status404 
            json $ object ["message" .= ("Nenhum produto encontrado com esse id" :: String)] 

updateProductName :: ActionM ()
updateProductName = do
    productId <- pathParam "id"
    body <- jsonData :: ActionM UpdateProductNameData
    let productName = Controllers.ProductController.name body
    updatedProduct <- liftIO $ runSqlite "apiHaskell.db" $ ProductServices.updateProductName productId productName
    case updatedProduct of
        Right p -> do
            status status200 
            json $ object ["products" .= p]
        Left ProductServices.InvalidRequest -> do
            status status400 
            json $ object ["message" .= ("Nome inválido" :: String)]
        Left ProductServices.ProductNotFound -> do
            status status404
            json $ object ["message" .= ("Produto não encontrado" :: String)]

updateProductBrand :: ActionM ()
updateProductBrand = do
    productId <- pathParam "id"
    body <- jsonData :: ActionM UpdateProductBrandData
    let productBrand = Controllers.ProductController.brand body
    updatedProduct <- liftIO $ runSqlite "apiHaskell.db" $ ProductServices.updateProductBrand productId productBrand
    case updatedProduct of
        Right p -> do                      
            status status200
            json $ object ["products" .= p]
        Left ProductServices.InvalidRequest -> do
            status status400 
            json $ object ["message" .= ("Marca inválida" :: String)]
        Left ProductServices.ProductNotFound -> do
            status status404
            json $ object ["message" .= ("Produto não encontrado" :: String)] 

updateProductPrice :: ActionM ()
updateProductPrice = do
    productId <- pathParam "id"
    body <- jsonData :: ActionM UpdateProductPriceData
    let productPrice = Controllers.ProductController.price body
    updatedProduct <- liftIO $ runSqlite "apiHaskell.db" $ ProductServices.updateProductPrice productId productPrice
    case updatedProduct of
        Right p -> do                      
            status status200
            json $ object ["products" .= p]
        Left ProductServices.InvalidRequest -> do
            status status400 
            json $ object ["message" .= ("Preço inválido" :: String)]
        Left ProductServices.ProductNotFound -> do
            status status404
            json $ object ["message" .= ("Produto não encontrado" :: String)]
