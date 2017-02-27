module Main where

import Codec.Picture

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Time

import qualified Drawhub.Github as G

import Git
import Git.Libgit2 (lgFactory)

import System.Environment
import System.Exit

readArgs :: [String] -> Maybe (FilePath, FilePath)
readArgs (x:y:_) = Just (x, y)
readArgs _ = Nothing

handleError :: Either String a -> IO a
handleError (Left msg) = putStrLn ("Error: " ++ msg) >> exitFailure
handleError (Right a) = return a

handleMaybe :: Maybe a -> IO a
handleMaybe (Just x) = return x
handleMaybe Nothing = putStrLn "Error: Nothing" >> exitFailure

repoOptions :: FilePath -> RepositoryOptions
repoOptions path = RepositoryOptions {
    repoPath = path,
    repoWorkingDir = Nothing,
    repoIsBare = False,
    repoAutoCreate = True
}

mutate :: MonadGit r m => Maybe (Tree r) -> (TreeT r m a -> m (TreeOid r))
mutate Nothing = createTree
mutate (Just t) = mutateTree t

createDummyChange :: (Show a, MonadGit r m) => Maybe (Tree r) -> a -> m (TreeOid r)
createDummyChange tree n = do
    let buffer = BlobString . encodeUtf8 . T.pack . show $ n
    blobID <- createBlob buffer
    mutate tree $ putEntry (encodeUtf8 . T.pack $ "dummy") BlobEntry {
        blobEntryOid = blobID,
        blobEntryKind = PlainBlob
    }

getSignature :: String -> ZonedTime -> Signature
getSignature mail time = Signature {
    signatureName = T.pack mail,
    signatureEmail = T.pack mail,
    signatureWhen = time
}

main :: IO ()
main = do
    args <- getArgs
    (inputPath, outputPath) <- handleMaybe $ readArgs args
    image <- convertRGB8 <$> (readImage inputPath >>= handleError)
    savePngImage outputPath (ImageRGB8 . G.getRgbImage $ G.fitImage image)

    utc <- getCurrentTime
    tz <- getCurrentTimeZone
    let now = utcToZonedTime tz utc

    let mail = "truc.machin@bidule.com"

    withRepository' lgFactory (repoOptions "/tmp/tmp") $ do
        let parent = Nothing
        let n = 1
        let date = now
        tree <- createDummyChange Nothing n

        let sig = getSignature mail date
        let commitMessage = T.pack $ "commit " ++ show n

        createCommit (commitOid <$> toList parent) tree sig sig commitMessage Nothing
    print "Finished"
