{-# LANGUAGE OverloadedStrings #-}

module Routes.ProductsRoutes where

import Web.Scotty

import Controllers.ProductController

productsRoutes :: ScottyM ()
productsRoutes = do
    get "/products" $ getAllProducts
    get "/products/:id" $ getProductById
    post "/products" $ createProduct
    delete "/products/:id" $ deleteProduct
    patch "/products/:id/name" $ updateProductName
    patch "/products/:id/brand" $ updateProductBrand
    patch "/products/:id/price" $ updateProductPrice
