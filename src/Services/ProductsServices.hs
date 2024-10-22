module Services.ProductsServices where

import Control.Monad.IO.Class()
import Control.Monad.IO.Class
import Database.Persist.TH()
import Database.Persist.Sqlite
import Control.Monad()
import Data.Int (Int64) 

import Models.Product
import Util.Validators

-- Tipo de dados para passar no either
-- O compilador estava acusando que eu tinha estorouado o limite de pattern match, então troquei string por uma estrutura de dados
-- Pattern match checker ran into -fmax-pmcheck-models=30 limit, so
--      • Redundant clauses might not be reported at all
--      • Redundant clauses might be reported as inaccessible
--      • Patterns reported as unmatched might actually be matched
-- Suggested fix:
--      Increase the limit or resolve the warnings to suppress this message.
-- Solução gerada por IA
data UpdateError = InvalidRequest | ProductNotFound
    deriving (Show)

-- Função para arredondar número para x casas decimais
-- Obtida em: https://www.tutorialspoint.com/haskell-program-to-round-a-number-to-n-decimal-places
roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

getAllProducts :: MonadIO m => SqlPersistT m [Products]
getAllProducts = do
    entities <- selectList [] []
    return $ map entityVal entities
    
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
        let newProduct = Products (getName p) (getBrand p) (roundTo 2 $ getPrice p)
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

updateProductName :: MonadIO m => Int64 -> String -> SqlPersistT m (Either UpdateError (Key Products, Products))
updateProductName productIdInt newName = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do
            if not (isValidName newName)
                then return $ Left InvalidRequest
                else do
                    updatedProduct <- updateGet productId [ProductsName =. newName]
                    return $ Right (productId, updatedProduct)
        Nothing -> return $ Left ProductNotFound

updateProductBrand :: MonadIO m => Int64 -> String -> SqlPersistT m (Either UpdateError (Key Products, Products))
updateProductBrand productIdInt newBrand = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do
            if not (isValidBrand newBrand)
                then return $ Left InvalidRequest
                else do
                    updatedProduct <- updateGet productId [ProductsBrand =. newBrand]
                    return $ Right (productId, updatedProduct)
        Nothing -> return $ Left ProductNotFound

updateProductPrice :: MonadIO m => Int64 -> Double -> SqlPersistT m (Either UpdateError (Key Products, Products))
updateProductPrice productIdInt newPrice = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do
            if not (isValidPrice newPrice)
                then return $ Left InvalidRequest
                else do
                    let roundedNumber = roundTo 2 newPrice
                    updatedProduct <- updateGet productId [ProductsPrice =. roundedNumber]
                    return $ Right (productId, updatedProduct)
        Nothing -> return $ Left ProductNotFound
