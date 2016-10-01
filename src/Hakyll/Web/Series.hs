{-# LANGUAGE LambdaCase #-}

-- | Module for adding series functionality to a blog, similar to tags.

module Hakyll.Web.Series
  ( seriesField
  , getSeries
  , buildSeries
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.List           (elemIndex)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Hakyll

-- | Gets the series from an identifier. Similar to 'getTags',
-- except it only accepts one series per identifier.
getSeries :: MonadMetadata m => Identifier -> m (Maybe String)
getSeries = flip getMetadataField "series"

toAlt :: (Foldable f, Alternative m) => f a -> m a
toAlt = foldr ((<|>) . pure) empty

infixr 1 >->
(>->) :: Functor f =>  (a -> f b) -> (b -> c) -> a -> f c
f >-> g = f >>> fmap g

-- | Generates four fields:
--
--    * series: The name of the series
--
--    * seriesLength: The total number of posts in the series
--
--    * seriesCurPos: The position of the current post in the series
--
--    * seriesUrl: The URL of the series page

seriesField :: Tags -> Context a
seriesField tags = Context $ const . \case
    "series"       -> seriesName
                  >-> StringField

    "seriesCurPos" -> itemIdentifier &&& otherPostsInSeries
                  >>> sequence
                  >>> fmap (uncurry elemIndex)
                  >=> toAlt
                  >-> succ
                  >>> show
                  >>> StringField

    "seriesLength" -> otherPostsInSeries
                  >-> length
                  >>> show
                  >>> StringField

    "seriesUrl"    -> seriesName
                  >=> tagsMakeId tags
                  >>> getRoute
                  >=> toAlt
                  >-> toUrl
                  >>> StringField

    _ -> const empty
  where
    seriesName = itemIdentifier
             >>> getSeries
             >=> toAlt
    otherPostsInSeries = seriesName
                     >=> flip lookup (tagsMap tags)
                     >>> toAlt

-- | Similar to the 'buildTags' function in "Hakyll.Web.Tags", except
-- checks the series field, and can only accept one series per item.
buildSeries :: MonadMetadata m
            => Pattern
            -> (String -> Identifier) -- ^ Function for converting a given series name into an identifier for its page
            -> m Tags
buildSeries pattrn makeId = do
    ids <- getMatches pattrn
    tagMap <- foldM addTags Map.empty ids
    let set' = Set.fromList ids
    inOrder <- (traverse.traverse) sortChronological (Map.assocs tagMap)
    pure $ Tags inOrder makeId (PatternDependency pattrn set')
  where
    addTags tagMap id' =
        maybe tagMap (\k -> Map.insertWith (++) k [id'] tagMap) <$> getSeries id'
