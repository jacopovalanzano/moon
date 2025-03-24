{-# LANGUAGE OverloadedStrings #-}
module Moon.Routing.Router (route, routeS, genHtml, genJson, Web.Scotty.header, Web.Scotty.param, Web.Scotty.request) where

import Web.Scotty (ActionM, ScottyM, get, post, put, delete, options, html, json, literal, capture, request, header, param)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

{- Define the RouteHandler type -}
type RouteHandler = Web.Scotty.ActionM ()
{- Define the Route type -}
type Route = String -> String -> RouteHandler -> Web.Scotty.ScottyM ()

{- Define the route method -}
route :: Route
route method path handler = do
    case method of
        "get" -> Web.Scotty.get (capture path) handler
        "post" -> Web.Scotty.post (capture path) handler
        "put" -> Web.Scotty.put (capture path) handler
        "delete" -> Web.Scotty.delete (capture path) handler
        "options" -> Web.Scotty.options (capture path) handler
        _     -> error "Unsupported HTTP method"  -- Add more cases for POST, PUT, etc.

{- Define the static route method:
   a static route uses the "literal" function rather than the "capture" function
-}
routeS :: Route
routeS method path handler = do
    case method of
        "get"    -> Web.Scotty.get (literal path) $ handleRequest handler
        "post"   -> Web.Scotty.post (literal path) $ handleRequest handler
        "put"    -> Web.Scotty.put (literal path) $ handleRequest handler
        "delete" -> Web.Scotty.delete (literal path) $ handleRequest handler
        "options" -> Web.Scotty.options (literal path) $ handleRequest handler
        _        -> error "Unsupported HTTP method"

{- Define the handleRequest method; in the future, the route handler can be expanded -}
handleRequest :: RouteHandler -> Web.Scotty.ActionM ()
handleRequest handler = do
    handler

{- Generates an HTML response -}
genHtml :: TL.Text -> Web.Scotty.ActionM ()
genHtml content = do
    html $ content

{- Generates a JSON response; might have to replace with a lazy version -}
genJson :: T.Text -> Web.Scotty.ActionM ()
genJson content = do
    -- Send the JSON content as the response
    json $ content

{- Retrieve the value of the request header -}
getHeader :: TL.Text -> Web.Scotty.ActionM (T.Text)
getHeader headerName = do
    userAgent <- Web.Scotty.header headerName
    return $ maybe "unknown" TL.toStrict userAgent

{- Retrieve the value of a URL parameter -}
getUrlParameter :: TL.Text -> Web.Scotty.ActionM (T.Text)
getUrlParameter = Web.Scotty.param