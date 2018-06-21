{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Component(Component(..), componentTarget, dispComponent) where

import Cabal.Plan
import Control.Applicative (empty)
import Data.Binary (Binary(..), Get, Put)
import Data.Text (Text)
import qualified Data.Text as T

data Component = Component
    { pkgid :: !PkgId
    , compname :: !CompName
    } deriving (Eq, Ord, Show)

dispComponent :: Component -> Text
dispComponent Component{..} = dispPkgId pkgid <> ":" <> dispCompName compname

componentTarget :: Component -> String
componentTarget Component{..} = T.unpack $ case compname of
        CompNameLib -> componentName <> ":" <> pkgName
        _ -> componentName
  where
    PkgId (PkgName pkgName) _version = pkgid

    componentName :: Text
    componentName = ":pkg:" <> pkgName <> ":" <> dispCompName compname

instance Binary Component where
    put (Component (PkgId (PkgName pkgName) (Ver version)) compName) = do
        put pkgName
        putList version
        case compName of
            CompNameLib -> putString "lib"
            CompNameSetup -> putString "setup"
            CompNameSubLib t -> putString "sublib" >> put t
            CompNameFLib t -> putString "flib" >> put t
            CompNameExe t -> putString "exe" >> put t
            CompNameTest t -> putString "test" >> put t
            CompNameBench t -> putString "bench" >> put t
      where
        putString :: String -> Put
        putString = put

    get = do
        pkgName <- PkgName <$> get
        version <- Ver <$> get
        tag <- get :: Get String
        Component (PkgId pkgName version) <$> case tag of
            "lib" -> return CompNameLib
            "setup" -> return CompNameSetup
            "sublib" -> CompNameSubLib <$> get
            "flib" -> CompNameFLib <$> get
            "exe" -> CompNameExe <$> get
            "test" -> CompNameTest <$> get
            "bench" -> CompNameBench <$> get
            _ -> empty
