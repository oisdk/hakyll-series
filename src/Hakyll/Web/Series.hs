{-# LANGUAGE LambdaCase #-}

-- | Module for adding series functionality to a blog, similar to tags.

module Hakyll.Web.Series
  ( seriesField
  , getSeries
  , buildSeries
  , compileSeries
  , SeriesInfo(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.List           (elemIndex)
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           Hakyll
import           Prelude             hiding (head)

-- | Gets the series from an identifier. Similar to 'getTags',
-- except it only accepts one series per identifier.
getSeries :: MonadMetadata m => Identifier -> m (Maybe String)
getSeries = flip getMetadataField "series"

-- | Compiles all of the information available on a given series
compileSeries :: String              -- ^ Name of series
              -> Tags                -- ^ Collected series
              -> Identifier          -- ^ Thing which is a member of a series
              -> Compiler SeriesInfo
compileSeries serie tags ident = do
  otherPostsInSeries <- toAlt (lookup serie (tagsMap tags))
  let seriesLen = length otherPostsInSeries
  curInd <- toAlt (elemIndex ident otherPostsInSeries)
  let curNum = curInd + 1
  loc <- toAlt =<< getRoute (tagsMakeId tags serie)
  pure $ SeriesInfo seriesLen curNum serie (toUrl loc)

-- | This represents the information available in an item
-- in a series, for display.
data SeriesInfo = SeriesInfo
  { seriesLength :: Int    -- ^ The total length of the series
  , seriesCurPos :: Int    -- ^ The number of the current post in this series
  , seriesName   :: String -- ^ The name of the series
  , seriesUrl    :: String -- ^ The location of the aggregated posts
  }

toAlt :: Alternative f => Maybe a -> f a
toAlt = maybe empty pure

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
    "series"       -> getSeriesField seriesName
    "seriesCurPos" -> getSeriesField (show . seriesCurPos)
    "seriesLength" -> getSeriesField (show . seriesLength)
    "seriesUrl"    -> getSeriesField seriesUrl
    _              -> const empty
  where
    getSeriesField :: (SeriesInfo -> String) -> Item a -> Compiler ContextField
    getSeriesField disp item = do
      serie <- getSeries ident >>= maybe empty pure
      seriesInfo <- compileSeries serie tags ident
      pure (StringField (disp seriesInfo))
      where ident = itemIdentifier item

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
