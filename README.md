# hakyll-series

Module for adding series functionality to hakyll.

In your posts, provide metadata at the top like so:

```markdown
---
title: something
series: things
---
```

This will add the following fields to the post:

    * series: The name of the series

    * seriesLength: The total number of posts in the series

    * seriesCurPos: The position of the current post in the series

    * seriesUrl: The URL of the series page

Using that, in your post template, something like this:


```html
$if(series)$
    <a href="$seriesUrl$">Part $seriesCurPos$ of a $seriesLength$-part series on $series$</a>
$endif$
```

Will render like this:

> Part 1 of a 5-part series on things

Linked to the aggregate page for the series.

> Examples
>
>   Part 1: something

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
postCtxWithSeries series = seriesField series `mappend` postCtx
```

A minimal example is provided in this repo, on top of the default hakyll setup. (it also provides the templates)
