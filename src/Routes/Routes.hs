{-# LANGUAGE OverloadedStrings #-}

module Routes.Routes where

import Web.Scotty
import Data.Text.Lazy (pack)

import Routes.ProductsRoutes(productsRoutes)

routes :: ScottyM ()
routes = do
    get "/" $ do
        htmlFile <- liftIO $ readFile "./src/Public/Html/index.html"
        html $ mconcat [pack htmlFile]
    productsRoutes
