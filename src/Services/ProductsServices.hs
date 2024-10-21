module Services.ProductsServices where

import Control.Monad.IO.Class()
import Control.Monad.IO.Class
import Database.Persist.TH()
import Database.Persist.Sqlite
import Control.Monad()
import Data.Int (Int64) 

import Models.Product
import Util.Validators

getAllProducts :: MonadIO m => SqlPersistT m [(Key Products, Products)]
getAllProducts = do
    entities <- selectList [] []
    return $ map (\entity -> (entityKey entity, entityVal entity)) entities

getProductById :: MonadIO m => Int64 -> SqlPersistT m (Maybe Products)
getProductById productIdInt = do
    let productId = toSqlKey productIdInt :: Key Products
    get productId

createProduct :: MonadIO m => ProductData -> SqlPersistT m (Maybe (Key Products, Products))
createProduct p
    | not $ isValidName $ getName p = return Nothing
    | not $ isValidBrand $ getBrand p = return Nothing
    | not $ isValidPrice $ getPrice p = return Nothing
    | otherwise = do
        let newProduct = Products (getName p) (getBrand p) (getPrice p)
        productId <- insert newProduct
        return $ Just (productId, newProduct)

deleteProduct :: MonadIO m => Int64 -> SqlPersistT m Bool
deleteProduct productIdInt = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do                        -- "Just _ -> ..." qualquer valor, desde que exista
            delete productId
            return True
        Nothing -> return False

updateProductName :: MonadIO m => Int64 -> String -> SqlPersistT m (Maybe (Key Products, Products))
updateProductName productIdInt newName = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do
            if not (isValidName newName)
                then return Nothing
                else do
                    updatedProduct <- updateGet productId [ProductsName =. newName]
                    return $ Just (productId, updatedProduct)
        Nothing -> return Nothing

updateProductBrand :: MonadIO m => Int64 -> String -> SqlPersistT m (Maybe (Key Products, Products))
updateProductBrand productIdInt newBrand = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do
            if not (isValidBrand newBrand)
                then return Nothing
                else do
                    updatedProduct <- updateGet productId [ProductsBrand =. newBrand]
                    return $ Just (productId, updatedProduct)
        Nothing -> return Nothing

updateProductPrice :: MonadIO m => Int64 -> Double -> SqlPersistT m (Maybe (Key Products, Products))
updateProductPrice productIdInt newPrice = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do
            if not (isValidPrice newPrice)
                then return Nothing
                else do
                    updatedProduct <- updateGet productId [ProductsPrice =. newPrice]
                    return $ Just (productId, updatedProduct)
        Nothing -> return Nothing