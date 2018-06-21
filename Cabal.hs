{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Cabal (checkComponent, createCabalReplProc, createPlanJson) where

import Control.Applicative ((<|>), many)
import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Attoparsec.Text
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Exit (ExitCode(..))
import System.IO (Handle, IOMode(..), hPutStrLn, stderr, withFile)
import System.Process
    (CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)

import Component
import Paths_ghc_check (getDataFileName)

data TmpException = TmpException deriving (Show)

instance Exception TmpException

data RebuildState = Reload | Restart
    deriving (Eq, Show)

runProcess :: MonadIO m => CreateProcess -> m Text
runProcess createProc = liftIO $ do
    (Nothing, Just stdout, Nothing, hnd) <- withDevNull $ \devNull -> do
        createProcess
            createProc{ std_out = CreatePipe, std_err = UseHandle devNull }

    waitForProcess hnd >>= \case
        ExitSuccess -> return ()
        e -> do
            hPutStrLn stderr "cabal-install exited with error"
            throwIO e

    T.hGetContents stdout
  where
    withDevNull :: (Handle -> IO a) -> IO a
    withDevNull = withFile "/dev/null" WriteMode

createPlanJson :: MonadIO m => m ()
createPlanJson = void . runProcess $ proc "cabal" ["new-build", "all", "--dry"]

createCabalReplProc :: MonadIO m => Component -> [String] -> m CreateProcess
createCabalReplProc comp extraArgs = do
    fp <- liftIO $ getDataFileName "ghc-check.ghci"
    return . proc "cabal" $ mconcat
        [ ["new-repl", componentTarget comp]
        , extraArgs
        , [ "--repl-options=-ignore-dot-ghci"
          , "--repl-options=-ghci-script=" ++ fp
          ]
        ]

checkComponent :: MonadIO m => Component -> m a -> m a -> m a
checkComponent target reset reload = do
    output <- createCabalReplProc target ["--dry"] >>= runProcess

    case parseOnly (noUpdates <|> componentUpdates) output of
        Left _ -> liftIO $ throwIO TmpException
        Right updates
            | checkRebuildStates updates -> reload
            | otherwise -> reset
  where
    checkRebuildStates :: Map Text RebuildState -> Bool
    checkRebuildStates = M.null . M.update toggle (dispComponent target)

    toggle :: RebuildState -> Maybe RebuildState
    toggle action = case action of
        Reload -> Nothing
        Restart -> Just Restart

skipLine :: Parser ()
skipLine = skipWhile (not . isEndOfLine) <* endOfLine

noUpdates :: Parser (Map Text RebuildState)
noUpdates = M.empty <$ string "Up to date" <* endOfLine <* endOfInput

componentUpdates :: Parser (Map Text RebuildState)
componentUpdates = do
    option () $ do
        string "Warning: The package list" *> skipLine
        string "Run 'cabal update'" *> skipLine

    option () $ do
        string "Resolving dependencies..." *> endOfLine

    string "Build profile:" *> skipLine
    string "In order, the following" *> skipLine
    mconcat <$> many outOfDateComponent <* endOfInput

outOfDateComponent :: Parser (Map Text RebuildState)
outOfDateComponent = do
    string " - "
    package <- takeWhile1 (/=' ') <* char ' '
    component <- char '(' *> takeWhile1 (/=')') <* string ") "
    action <- char '(' *> rebuildReason <* char ')' <* endOfLine
    return $ M.singleton (package <> ":" <> component) action

rebuildReason :: Parser RebuildState
rebuildReason = choice
    [ Reload <$ string "first run"
    , Reload <$ string "existing package"
    , Reload <$ string "already installed"
    , Reload <$ string "ephemeral targets"
    , Reload <$ string "up to date"
    , Reload <$ do
        string "file "
        manyTill anyChar (string " changed")
    , Restart <$ skipWhile (/=')')
    ]
