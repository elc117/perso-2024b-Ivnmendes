{-# LANGUAGE OverloadedStrings #-}

module Routes.Routes where

import Web.Scotty

import Routes.ProductsRoutes(productsRoutes)

routes :: ScottyM ()
routes = do
    get "/" $ html "Hello, world!"
    get "/about" $ html "About page"
    productsRoutes
