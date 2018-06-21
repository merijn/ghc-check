{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import Prelude hiding (getContents)
import Cabal.Plan (findProjectRoot)
import Control.Concurrent.Chan
import Control.Exception (handleJust)
--import qualified Data.ByteString as BS
--import Data.ByteString.Lazy.Internal (ByteString(..), defaultChunkSize)
import Data.Binary(decodeFile, encodeFile)
import Data.Maybe (maybe)
import Data.Version
import Options.Applicative
--import Network.Socket ()
--import Network.Socket.ByteString (recv)
import System.Exit
import System.Directory
import System.FilePath
import System.IO hiding (getContents)
import System.IO.Error (isDoesNotExistError)
--import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Monad (forM_, guard)
import Language.Haskell.Ghcid (Load(..))

import Checker
import Paths_ghc_check

type CacheFun
     = FilePath
    -> (Maybe ProjectState -> IO (Result, ProjectState))
    -> IO Result

data Checker = Checker
    { checkerVerbosity :: Verbosity
    , checkerCommand :: Command
    }

data Command
    = Root FilePath
    | Daemon FilePath CacheFun
    | Check FilePath CacheFun

--cmdParser :: ParserInfo Checker
cmdParser = info (Checker <$> verbosity <*> commands <**> helper) $ mconcat
    [ fullDesc
    , header $ "ghc-check v" ++ showVersion version
    ]
  where
    verbosity :: Parser Verbosity
    verbosity = (verbosities !!) <$> intVerbosity
      where
        verbosities :: [Verbosity]
        verbosities = LogSilent : LogError : repeat LogDebug

        intVerbosity :: Parser Int
        intVerbosity = length . take 2 <$> vFlag <|> vOption

        vFlag :: Parser [()]
        vFlag = many . flag' () . mconcat $
            [ short 'v', long "verbose", hidden ]

        vOption :: Parser Int
        vOption = option auto . mconcat $
            [ short 'v', long "verbose", help "Enable more verbose logging."
            , value 0, metavar "N", showDefault ]

    commands :: Parser Command
    commands = hsubparser $ mconcat
        [ command "root" . info rootParser $
            progDesc "Output the project root for a file"
        , command "daemon" . info daemonParser $
            progDesc "Start a checking daemon in directory"
        , command "check" . info checkParser $
            progDesc "Check a file"
        ]
      where
        rootParser :: Parser Command
        rootParser = Root <$> strArgument (mconcat [metavar "PATH"])

        daemonParser :: Parser Command
        daemonParser = Daemon
            <$> strArgument (mconcat [metavar "PATH"])
            <*> cacheFlag

        checkParser :: Parser Command
        checkParser = Check
            <$> strArgument (mconcat [metavar "PATH"])
            <*> cacheFlag

        cacheFlag :: Parser CacheFun
        cacheFlag = flag withCachedState (\_ f -> fst <$> f Nothing) $ mconcat
            [ long "no-cache" ]

withCachedState
    :: FilePath -> (Maybe ProjectState -> IO (a, ProjectState)) -> IO a
withCachedState projRoot work = do
    projState <- handleMissing Nothing $ Just <$> decodeFile cacheFile
    (result, pj) <- work projState
    if isEmptyProjectState pj
       then handleMissing () $ removeFile cacheFile
       else encodeFile cacheFile pj
    return result
  where
    cacheFile :: FilePath
    cacheFile = projRoot </> ".ghc-check.cache"

    handleMissing :: a -> IO a -> IO a
    handleMissing = handleJust (guard . isDoesNotExistError) . const . return

doDaemon :: IO (IO CheckerCommand)
doDaemon = return $ getLine >>= \case
    [] -> return Quit
    file -> return $ CheckFile file printErrors

doCheck :: FilePath -> IO (IO CheckerCommand)
doCheck fp = do
    chan <- newChan
    writeChan chan (CheckFile fp printErrors)
    writeChan chan Quit
    return (readChan chan)

reportRoot :: FilePath -> IO FilePath
reportRoot fp = do
    isDir <- doesDirectoryExist fp
    let dir = if isDir then fp else dropFileName fp
    findProjectRoot dir >>= maybe exitFailure return

printErrors :: [Load] -> IO ()
printErrors l = forM_ [loadMessage | Message{..} <- l] $ putStrLn . unlines

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr NoBuffering
    Checker{..} <- execParser cmdParser
    let check = withChecker checkerVerbosity
    case checkerCommand of
        Root sourceFile -> reportRoot sourceFile >>= putStrLn
        Daemon sourceFile withCache -> do
            root <- reportRoot sourceFile
            doDaemon >>= withCache root . check root >>= \case
                Done -> return ()
                Fail -> exitFailure
        Check sourceFile withCache -> do
            root <- reportRoot sourceFile
            doCheck sourceFile >>= withCache root . check root >>= \case
                Done -> return ()
                Fail -> exitFailure

{-
runDaemon :: FilePath -> IO ()
runDaemon projRoot = withCurrentDirectory projRoot $ do
    bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
        bind sock (SockAddrUnix sockPath)
        listen sock 10
        forever . bracket (fmap fst $ accept sock) close $ \clientSock -> do
            undefined
  where
    sockPath = projRoot </> ".ghc-check"

getContents
    :: Socket
    -> IO ByteString
getContents sock = loop where
  loop = unsafeInterleaveIO $ do
    s <- recv sock defaultChunkSize
    if BS.null s
      then return Empty
      else Chunk s `liftM` loop
      -}
