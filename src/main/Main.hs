module Main where

import Codec.Picture

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock

import Control.Monad

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

iterateM :: Monad m => (a -> m a) -> m a -> [m a]
iterateM f u = u : iterateM f (u >>= f)

iterateN :: Monad m => Int -> (a -> m a) -> m a -> m a
iterateN 0 _ x = x
iterateN n f x = iterateN (n - 1) f (x >>= f)

commitActivities :: MonadGit r m => [Int] -> (ZonedTime -> Signature) -> Day -> m (Maybe (Commit r))
commitActivities activities sign startDay = do
    (_, head, _) <- foldM (\ (day, parent, n) nbActivity -> do
        let zonedTime = utcToZonedTime utc $ UTCTime day (secondsToDiffTime 3600)
        let sig = sign zonedTime

        (commit, newN) <- commitNbActivities nbActivity sig parent n
        return (addDays 1 day, commit, newN)
        ) (startDay, Nothing, 0) activities
    return head

commitNbActivities :: MonadGit r m => Int -> Signature -> (Maybe (Commit r)) -> Int -> m (Maybe (Commit r), Int)
commitNbActivities nb sig parent n = iterateN nb commit (return (parent, n)) where
    head = (Just $ T.pack "HEAD")
    commit (curPar, curN) = do
        tree <- createDummyChange Nothing curN
        let message = T.pack $ "commit " ++ show curN
        newCommit <- createCommit (commitOid <$> toList curPar) tree sig sig message head
        return (return newCommit, curN + 1)

main :: IO ()
main = do
    args <- getArgs
    (inputPath, outputPath) <- handleMaybe $ readArgs args
    image <- convertRGB8 <$> (readImage inputPath >>= handleError)
    savePngImage outputPath (ImageRGB8 . G.getRgbImage $ G.fitImage image)

    let startDay = fromGregorian 2016 1 1
    let sign = getSignature "truc.machin@bidule.com"
    let activities = G.imageToNbActivities . G.fitImage $ image

    withRepository' lgFactory (repoOptions "/tmp/tmp") $ commitActivities activities sign startDay 
    print "Done"
