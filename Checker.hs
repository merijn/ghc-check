{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Checker
    ( CheckerCommand(..)
    , ProjectState(..)
    , unknownComponents
    , fileMappings
    , Result(..)
    , Verbosity(..)
    , isEmptyProjectState
    , withChecker) where

import Cabal.Plan
import Control.Monad.Catch (bracket, displayException, try)
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import Data.Binary (Binary)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Language.Haskell.Ghcid (Load(..), Severity(..), Stream(..))
import qualified Language.Haskell.Ghcid as Ghci
import Lens.Micro ((&), (.~), (%~))
import Lens.Micro.Mtl ((.=), (%=), use, zoom)
import Lens.Micro.TH (makeLenses)
import System.Directory (withCurrentDirectory)
import System.Exit (exitFailure)
import System.IO.Error (isUserError)
import System.IO (hPutStrLn, stderr)

import Cabal
import Component

data Verbosity = LogSilent | LogError | LogDebug deriving (Eq, Ord, Show)

data ProjectState = ProjectState
    { _fileMappings :: !(Map FilePath Component)
    , _unknownComponents :: !(Set Component)
    } deriving (Show, Generic)

instance Binary ProjectState

type CheckerM = StateT CheckerState IO

data CheckerState = CheckerState
    { _projectState :: ProjectState
    , _warningCache :: Map FilePath [Load]
    , _loadedFiles :: Set FilePath
    , _currentFile :: FilePath
    , _currentResponse :: [Load] -> CheckerM ()
    }

makeLenses ''ProjectState
makeLenses ''CheckerState

data CheckerCommand
    = CheckFile FilePath ([Load] -> IO ())
    | Quit

data Result = Fail | Done
    deriving (Eq, Show)

isEmptyProjectState :: ProjectState -> Bool
isEmptyProjectState ProjectState{..} =
  S.null _unknownComponents && M.null _fileMappings

deleteFindMin :: Ord s => Set s -> (Maybe s, Set s)
deleteFindMin s
    | S.null s = (Nothing, s)
    | otherwise = first Just $ S.deleteFindMin s

guessComponent :: CheckerM (Maybe Component)
guessComponent = M.lookup <$> use currentFile
                          <*> use (projectState . fileMappings)

updateProjectState :: MonadIO m => Verbosity -> ProjectState -> m ProjectState
updateProjectState verb pj = do
    comps <- localComponents verb
    return $
        pj & fileMappings %~ M.mapMaybe (checkMember comps)
           & unknownComponents .~ comps
  where
    checkMember :: Ord a => Set a -> a -> Maybe a
    checkMember set v = v <$ guard (S.member v set)

withChecker
    :: Verbosity
    -> FilePath
    -> IO CheckerCommand
    -> Maybe ProjectState
    -> IO (Result, ProjectState)
withChecker verb projRoot loadCmd mProjState =
  withCurrentDirectory projRoot $ do
    projState <- updateProjectState verb initProjectState
    loadCmd >>= \case
        Quit -> return (Done, projState)
        CheckFile fp resp -> do
            let checkerState = CheckerState projState M.empty S.empty fp (liftIO . resp)
                initComponent = fp `M.lookup` _fileMappings projState
                checkerLoop = componentsLoop initComponent
            (result, finalCheckerState) <- runStateT checkerLoop checkerState
            return (result, _projectState finalCheckerState)
  where
    initProjectState :: ProjectState
    initProjectState = fromMaybe (ProjectState M.empty S.empty) mProjState

    logMsg :: Stream -> String -> IO ()
    logMsg Stdout | verb >= LogDebug = putStrLn
    logMsg Stderr | verb >= LogError = putStrLn
    logMsg _ = const $ return ()

    componentsLoop :: Maybe Component -> CheckerM Result
    componentsLoop = queryComponent
        >=> maybe (return Fail) (ghciLoop >=> either return componentsLoop)

    queryComponent :: Maybe Component -> CheckerM (Maybe Component)
    queryComponent mComponent = zoom (projectState . unknownComponents) $ do
        case mComponent of
            component@(Just c) -> component <$ modify' (S.delete c)
            Nothing -> state deleteFindMin

    ghciLoop :: Component -> CheckerM (Either Result (Maybe Component))
    ghciLoop currComponent =
      bracket (runGhci logMsg currComponent) (liftIO . snd) $ updateLoop . fst
      where
        updateLoop :: CheckerM () -> CheckerM (Either Result (Maybe Component))
        updateLoop updateWarnings = loop
          where
            loop = do
                isLoaded <- S.member <$> use currentFile <*> use loadedFiles
                if isLoaded then reload else Right <$> guessComponent

            reload = checkComponent currComponent resetState $ do
                updateWarnings
                warnings <- M.findWithDefault [] <$> use currentFile
                                                 <*> use warningCache

                respond <- use currentResponse
                respond warnings

                liftIO loadCmd >>= \case
                    Quit -> return $ Left Done
                    CheckFile nextFile resp -> do
                        currentFile .= nextFile
                        currentResponse .= liftIO . resp
                        loop

        resetState :: CheckerM (Either Result (Maybe Component))
        resetState = zoom projectState $ Right (Just currComponent) <$ do
            get >>= updateProjectState verb >>= put

runGhci
    :: (Stream -> String -> IO ())
    -> Component
    -> CheckerM (CheckerM (), IO ())
runGhci logMsg activeComponent = do
    (ghci, msgs) <- liftIO $ do
        proc <- createCabalReplProc activeComponent []
        result@(ghci, _) <- Ghci.startGhciProcess proc logMsg
        Ghci.execStream ghci ":set -fno-force-recomp" logMsg
        return result

    loadedFiles .= S.fromList [loadFile | Loading{..} <- msgs]
    warningCache .= M.empty
    updateMappings msgs

    let lookupWarnings :: CheckerM ()
        lookupWarnings = liftIO (Ghci.reload ghci) >>= updateMappings

    return (lookupWarnings, Ghci.stopGhci ghci)
  where
    toMaps :: (Load -> v) -> [Load] -> [Map FilePath v]
    toMaps f = map $ \l -> M.singleton (loadFile l) (f l)

    toFileMappings :: [Load] -> Map FilePath Component
    toFileMappings = M.unions . toMaps (const activeComponent)

    toWarnings :: [Load] -> Map FilePath [Load]
    toWarnings = M.unionsWith (++) . toMaps (maybeToList . tidyMessage)

    updateMappings :: [Load] -> CheckerM ()
    updateMappings rawMsgs = do
        projectState . fileMappings %= M.union (toFileMappings msgs)
        warningCache %= M.union (toWarnings msgs)
      where
        msgs :: [Load]
        msgs = filter (not . isConfig) rawMsgs

        isConfig :: Load -> Bool
        isConfig LoadConfig{} = True
        isConfig _ = False

localComponents :: MonadIO m => Verbosity -> m (Set Component)
localComponents verb = liftIO $ do
    ePlan <- try $ findAndDecodePlanJson (ProjectRelativeToDir ".")
    components <- planToLocal <$> case ePlan of
        Right p -> return p
        Left e | isUserError e -> do
            createPlanJson
            findAndDecodePlanJson (ProjectRelativeToDir ".")
        Left e -> hPutStrLn stderr (displayException e) >> exitFailure

    when (verb >= LogDebug) . mapM_ print $ components
    return components
  where
    planToLocal :: PlanJson -> Set Component
    planToLocal = S.fromList . concatMap pkgToComponents . localPackages

    localPackages :: PlanJson -> [Unit]
    localPackages = filter isLocalPkg . M.elems . pjUnits

    isLocalPkg :: Unit -> Bool
    isLocalPkg = (==UnitTypeLocal) . uType

    pkgToComponents :: Unit -> [Component]
    pkgToComponents pkg = map (Component (uPId pkg)) components
      where
        components = M.keys (uComps pkg)

-- | Ignore entirely pointless messages and remove unnecessary lines.
tidyMessage :: Load -> Maybe Load
tidyMessage Message{loadSeverity=Warning, loadMessage=[_,x]}
    | x == "    -O conflicts with --interactive; -O ignored." = Nothing
tidyMessage m@Message{..}
    = Just m{ loadMessage = filterBad loadMessage }
  where
    filterBad = filter (\x -> not $ any (`isPrefixOf` x) bad)
    bad = ["      except perhaps to import instances from"
          ,"    To import instances alone, use: import "]
tidyMessage x = Just x
