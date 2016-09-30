# hakyll-series

Module for adding series functionality to hakyll.

In your posts, provide metadata at the top like so:

```markdown
---
title: something
series: things
---
```

In your templates, add something like this:

```html
$if(series)$
    $series$
$endif$
```

Then, hakyll will find the posts with the same info at the top, and render it like this:

> Part 3 from a 5-part series on examples

Which will link to a page like this:

> Examples
>
>   Part 1: ...

To add it to your blog, add this to your main:

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
