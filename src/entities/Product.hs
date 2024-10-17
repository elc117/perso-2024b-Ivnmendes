module Entities.Product where

data Product = Product
    {
    , name      :: String
    , brand     :: String
    , price     :: Float
    } deriving (Show, Eq)

getName :: Product -> String
getName product = name product

getBrand :: Product -> String
getBrand product = brand product

getPrice :: Product -> Float
getPrice product = price product