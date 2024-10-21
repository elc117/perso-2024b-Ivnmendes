{-# LANGUAGE OverloadedStrings #-}

module Routes.ProductsRoutes where

import Web.Scotty

productsRoutes :: ScottyM ()
productsRoutes = do
    get "/products" $ html "Nothing here :D"
