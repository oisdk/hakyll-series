-- | Module for adding series functionality to a blog, similar to tags.

module Hakyll.Web.Series
  ( seriesField
  , getSeries
  , buildSeries
  , renderSeries
  , SeriesInfo(..)
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.List                       (elemIndex)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe
import           Hakyll
import           Prelude                         hiding (head)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import qualified Data.Set as Set

-- | Gets the series from an identifier. Similar to 'getTags',
-- except it only accepts one series per identifier.
getSeries :: MonadMetadata m => Identifier -> m (Maybe String)
getSeries = fmap (removeEmpty . trim <=< Map.lookup "series") . getMetadata where
  removeEmpty [] = Nothing
  removeEmpty xs = Just xs

-- | Renders (with links) a given series.
renderSeries :: String                  -- ^ Name of series
             -> (SeriesInfo -> String)  -- ^ Function for displaying series info
             -> Tags                    -- ^ Collected series
             -> Identifier              -- ^ Thing which is a member of a series
             -> Maybe (Compiler String) -- ^ Returns nothing if the thing was not a member of a series
renderSeries serie desc tags ident = do
  otherPostsInSeries <- lookup serie (tagsMap tags)
  let seriesLen = length otherPostsInSeries
  curInd <- elemIndex ident otherPostsInSeries
  let curNum = curInd + 1
  let desc' = desc (SeriesInfo serie curNum seriesLen)
  let renderLink link = renderHtml $ H.a ! A.href (toValue $ toUrl link) $ toHtml desc'
  pure $ foldMap renderLink <$> getRoute (tagsMakeId tags serie)

-- | This represents the information available in an item
-- in a series, for display.
data SeriesInfo = SeriesInfo
  { seriesName   :: String -- ^ The name of the series
  , seriesLength :: Int    -- ^ The total length of the series
  , seriesCurPos :: Int    -- ^ The number of the current post in this series
  }

-- | Renders series with links.
seriesField :: (SeriesInfo -> String) -- ^ Custom rendering function for a series
            -> Tags                   -- ^ Collected series
            -> Context a
seriesField desc tags = field "series" $ \item -> do
    let ident = itemIdentifier item
    series <- getSeries ident
    fromMaybe (pure "") (series >>= \serie -> renderSeries serie desc tags ident)

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
