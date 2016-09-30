# hakyll-series

Module for adding series functionality to hakyll.

To add it to your blog, provide, in the metadata, a "series" field, with the name of the series. (each post can only belong to one.) Then, add this to your main:

```haskell
series <- buildSeries "posts/*" (fromCapture "series/*.html")

tagsRules series $ \serie pattrn -> do
    let title = toUpper (head serie) : tail serie
    route idRoute
    compile $ do
        posts <- chronological =<< loadAll pattrn
        let ctx = constField "title" title `mappend`
                  listField "posts" postCtx (pure posts) `mappend`
                  defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/series.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
```

To have access to the series context in each post, change the post rule to something like this:

```haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithSeries series)
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithSeries series)
        >>= relativizeUrls
```

Where `postCtxWithSeries` can be something like:

```haskell
postCtxWithSeries :: Tags -> Context String
postCtxWithSeries series = seriesField desc series `mappend` postCtx
  where
    desc (SeriesInfo serieName curNum seriesLen) = concat
      ["Part ", show curNum, " from a ", show seriesLen, "-part series on ", serieName]
```

A minimal example is provided in this repo, on top of the default hakyll setup. (it also provides the templates)
