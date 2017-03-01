module Main where

import Codec.Picture

import Control.Monad

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock

import qualified Drawhub.Github as G

import Git hiding (Options)
import Git.Libgit2 (lgFactory)

import Options.Applicative

import System.Environment
import System.Exit

data Options = OptImage {
        optInputImg :: FilePath,
        optOutputImg :: FilePath
    } | OptGit {
        optInputImg :: FilePath,
        optOutputGit :: FilePath,
        optMail :: String,
        optStartDay :: Day,
        optBare :: Bool
    }

optionsImage :: Parser Options
optionsImage = OptImage
    <$> strOption ( long "input" <> short 'i' <> help "Input path of image to process" )
    <*> strOption ( long "output" <> short 'o' <> help "Output path of generated image" )

optionsGit :: Parser Options
optionsGit = OptGit
    <$> strOption ( long "input" <> short 'i' <> help "Input path of image to process" )
    <*> strOption ( long "output" <> short 'o' <> help "Output path of git repository" )
    <*> strOption ( long "mail" <> short 'm' <> help "Mail of the committer (should be same as the Github account's" )
    <*> option dayReader ( long "start-day" <> short 'd' <> help "Start day of commits (should be a monday) (yyyy-mm-dd)" )
    <*> switch ( long "bare" <> short 'b' <> help "Create a bare git repository" )

options :: Parser Options
options = hsubparser
  $ command "image" (info optionsImage (progDesc "Transform an image to a preview"))
  <> command "commit" (info optionsGit (progDesc "Create a new repository with commits representing the image"))

dayReader :: ReadM Day
dayReader = eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" arg of
        Nothing -> Left ("Cannot parse date: " ++ arg)
        Just day -> Right day

main :: IO ()
main = doMain =<< execParser opts where
    opts = info (options <**> helper)
        (fullDesc <> progDesc desc <> header hdr)
    hdr = "Drawhub - Draw on Github"
    desc = "Draw an image on Github activities calendar"


doMain :: Options -> IO ()
doMain opts@(OptImage input output) = do
    img <- fitImage input
    savePngImage output (ImageRGB8 . G.getRgbImage $ img)

doMain opts@(OptGit input output mail startDay isBare) = do
    img <- fitImage input

    let repoOpt = repoOptions output isBare
    let sign = getSignature mail
    let activities = G.imageToNbActivities $ img

    withRepository' lgFactory repoOpt $ commitActivities activities sign startDay 
    return ()

fitImage :: FilePath -> IO G.GithubImage
fitImage path = G.fitImage . convertRGB8 <$> (handleError =<< readImage path)

handleError :: Either String a -> IO a
handleError (Left msg) = putStrLn ("Error: " ++ msg) >> exitFailure
handleError (Right a) = return a

repoOptions :: FilePath -> Bool -> RepositoryOptions
repoOptions path isBare = RepositoryOptions {
    repoPath = path,
    repoWorkingDir = Nothing,
    repoIsBare = isBare,
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

