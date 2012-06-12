module CustomSplices (addCustomSplices) where

import qualified Data.Text.Encoding as TE
import qualified Text.XmlHtml as X
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import Snap.Snaplet
import Snap.Snaplet.Heist
import Text.Templating.Heist

import Text.Templating.Heist.Splices.Apply

splitPathWith :: Char -> BC.ByteString -> [BC.ByteString]
splitPathWith s p = if BC.null p then [] else (reverse $ BC.split s path)
  where
    path = if BC.head p == s then BC.tail p else p

splitTPath :: BC.ByteString -> [BC.ByteString]
splitTPath = splitPathWith '/'

concatPathWith :: Char -> [BC.ByteString] -> BC.ByteString
concatPathWith _ [] = ""
concatPathWith s p  =
    foldl1 (\o n -> o `BC.append` (s `BC.cons` n)) $ reverse p

concatTPath :: [BC.ByteString] -> BC.ByteString
concatTPath = concatPathWith '/'

applyDir :: Monad m => T.Text -> Splice m
applyDir tempDir = do
  ts <- getTS
  mapSplices (applyNodes []) $
             map    (TE.decodeUtf8 . concatTPath) $
             filter (\(n:p) -> splitTPath tempDir' == p) $
             templateNames ts
      where
        tempDir' = TE.encodeUtf8 tempDir

applyDirImpl :: Monad m => Splice m
applyDirImpl = do
  n  <- getParamNode
  case X.getAttribute "template-dir" n of
    Nothing      -> return [] -- TODO: error handling
    Just tempDir -> applyDir tempDir

addCustomSplices :: HasHeist b => Initializer b v ()
addCustomSplices = addSplices [("applyDir", liftHeist applyDirImpl)]