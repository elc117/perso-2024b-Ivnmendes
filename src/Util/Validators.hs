module Util.Validators where

import Data.Char (isAlpha)

isValidName :: String -> Bool
isValidName name = not $ null name && all isAlpha name && (length name) > 3

isValidBrand :: String -> Bool
isValidBrand brand = not $ null brand && all isAlpha brand

isValidPrice :: Double -> Bool
isValidPrice price = price > 0