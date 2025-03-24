# Routing Module

The routing module is based on the [Scotty](https://hackage.haskell.org/package/scotty) web framework and 
provides a set of functions for handling HTTP requests, and generating HTML & JSON responses.

Use `Moon.Routing.Router.route` to generate a route (eg. '/test/:id') or `Moon.Routing.Router.routeS`
to generate a static route (eg. '/test' or '/test/1/2/3').

## Example usage

```haskell
-- moon/app/Main.hs
main :: IO ()
main = do

    ...

    -- TEST: create a simple HTML output
    let htmlTextLazy = Moon.Routing.Router.genHtml $ TL.concat
            ["<h1>Chivalry</h1>"
            ,"<p>Static Route Test Page</p>"
            ]

    -- Define static and dynamic routes
    let dynamicRoutes :: Web.Scotty.ScottyM ()
        dynamicRoutes = do
            -- Static routes
            Moon.Routing.Router.routeS "get" "/test" htmlTextLazy
            Moon.Routing.Router.routeS "get" "/test/static" $ Moon.Routing.Router.genHtml "<h1>Infantry</h1><p>Static Route Test Page</p>"

            -- Dynamic route with :id
            Moon.Routing.Router.route "get" "/test/:id" $ do
                id <- Moon.Routing.Router.param "id"  -- Extract :id from the URL
                Moon.Routing.Router.genHtml $ TL.concat
                    ["<h1>Chivalry</h1>"
                    ,"<p>ID: ", id, "</p>"
                    ]

            -- Static route (POST)
            Moon.Routing.Router.routeS "post" "/test" $ do
                -- Return a JSON response
                Moon.Routing.Router.genJson $ TL.toStrict $ TL.pack "{\"message\": \"Hello World\"}"

            -- Dynamic route
            Moon.Routing.Router.route "get" "/fab" $ do
                -- Get IP
                request_ <- Moon.Routing.Router.request
                let remoteIP = case Network.Wai.remoteHost request_ of
                        Network.Socket.SockAddrInet _ h -> T.pack $ show h -- h stands for "host"
                        Network.Socket.SockAddrInet6 _ _ h _ -> T.pack $ show h -- h stands for "host"
                        _ -> T.pack "unknown"  -- Use T.pack for Text

                -- Get User-Agent from headers
                userAgent_ <- Moon.Routing.Router.header (TL.pack "User-Agent")  -- TL.Text for header
                let userAgent = maybe (T.pack "unknown") TL.toStrict userAgent_  -- Convert to strict Text

                -- Generate HTML
                Moon.Routing.Router.genHtml $ TL.concat
                            [ TL.pack "<h1>Hello World</h1>"
                            , TL.pack "<p>IP: ", TL.fromStrict remoteIP, TL.pack "</p>"
                            , TL.pack "<p>User-Agent: ", TL.fromStrict userAgent, TL.pack "</p>"
                            ]
```

To dispatch the routes, use `Web.Scotty.scotty`

```haskell
-- moon/app/Main.hs
Web.Scotty.scotty 3000 dynamicRoutes
```