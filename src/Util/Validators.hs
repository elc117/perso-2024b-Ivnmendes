module Util.Validators where

isValidName :: String -> Bool
isValidName name = not (null name) && length name > 3

isValidBrand :: String -> Bool
isValidBrand brand = not (null brand) && (length brand) > 3

isValidPrice :: Double -> Bool
isValidPrice price = price > 0