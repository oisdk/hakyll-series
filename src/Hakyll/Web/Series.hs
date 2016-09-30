module Hakyll.Web.Series
  ( seriesField
  , buildSeries
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

getSeries :: MonadMetadata m => Identifier -> m (Maybe String)
getSeries =
  fmap (fmap trim . Map.lookup "series") . getMetadata

compileSeries :: String
              -> (SeriesInfo -> String)
              -> Tags
              -> Identifier
              -> Maybe (Compiler String)
compileSeries serie desc tags ident = do
  otherPostsInSeries <- lookup serie (tagsMap tags)
  let seriesLen = length otherPostsInSeries
  curInd <- elemIndex ident otherPostsInSeries
  let curNum = curInd + 1
  let desc' = desc (SeriesInfo serie curNum seriesLen)
  let renderLink link = renderHtml $ H.a ! A.href (toValue $ toUrl link) $ toHtml desc'
  pure $ foldMap renderLink <$> getRoute (tagsMakeId tags serie)


data SeriesInfo = SeriesInfo
  { seriesName   :: String -- ^ The name of the series
  , seriesLength :: Int    -- ^ The total length of the series
  , seriesCurPos :: Int    -- ^ The number of the current post in this series
  }

-- | Builds a field from tags and a stringify function
seriesField :: (SeriesInfo -> String) -> Tags -> Context a
seriesField desc tags = field "series" $ \item -> do
    let ident = itemIdentifier item
    series <- getSeries ident
    fromMaybe (pure "") (series >>= \serie -> compileSeries serie desc tags ident)

-- | Similar to the @buildTags@ function in @Hakyll.Web.Tags@, except
-- checks the series field, and can only accept one series per item.
buildSeries :: MonadMetadata m
            => Pattern
            -> (String -> Identifier)
            -> m Tags
buildSeries pattrn makeId = do
    ids    <- getMatches pattrn
    tagMap <- foldM addTags Map.empty ids
    let set' = Set.fromList ids
    inOrder <- (traverse.traverse) sortChronological (Map.assocs tagMap)
    pure $ Tags inOrder makeId (PatternDependency pattrn set')
  where
    addTags tagMap id' =
        maybe tagMap (\k -> Map.insertWith (++) k [id'] tagMap) <$> getSeries id'
