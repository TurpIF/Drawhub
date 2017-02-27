module Main where

import Codec.Picture

import Data.ByteString (ByteString)
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

main :: IO ()
main = do
    args <- getArgs
    (inputPath, outputPath) <- handleMaybe $ readArgs args
    image <- convertRGB8 <$> (readImage inputPath >>= handleError)
    savePngImage outputPath (ImageRGB8 . G.getRgbImage $ G.fitImage image)

    utc <- getCurrentTime
    tz <- getCurrentTimeZone
    let now = utcToZonedTime tz utc

    withRepository' lgFactory (repoOptions "/tmp/tmp") $ do
        let buffer = BlobString . encodeUtf8 . T.pack $ "test"

        blobID <- createBlob buffer
        tree <- createTree $ putEntry (encodeUtf8 . T.pack $ "useless") BlobEntry {
            blobEntryOid = blobID,
            blobEntryKind = PlainBlob
        }

        let sig = Signature {
                signatureName = T.pack "Nobody",
                signatureEmail = T.pack "nobody@example.com",
                signatureWhen = now
            }

            commitMessage :: T.Text
            commitMessage = T.pack "Dummy message"

        mRef <- lookupReference (T.pack "HEAD")
        let ref = fromMaybe (error "Invalid ref: HEAD") mRef

        --mCid <- referenceToOid ref
        --let cid = Tagged $ fromMaybe (error "Something bad happened") mCid

        createCommit [] tree sig sig commitMessage (Just $ T.pack "HEAD")
    print "Finished"
