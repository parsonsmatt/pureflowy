{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the code that is used for generating the PureScript
-- client code.
module PureFlowy.Client where

import           Control.Applicative
import           Control.Lens
import           Data.Proxy
import           Language.PureScript.Bridge
import           Servant.PureScript

import PureFlowy.Api.Todos (Api, Todo)

-- | We have been lazy and defined our types in the WebAPI module,
--   we use this opportunity to show how to create a custom bridge moving those
--   types to Counter.ServerTypes.
fixTypesModule :: BridgePart
fixTypesModule = do
    typeModule ^== "PureFlowy.Api"
    t <- view haskType
    TypeInfo (_typePackage t) "Counter.ServerTypes" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = defaultBridge <|> fixTypesModule

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
    languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =
    [ mkSumType (Proxy :: Proxy Todo)
    ]

mySettings :: Settings
mySettings = defaultSettings & apiModuleName .~ "PureFlowy.Api"

generateClient :: IO ()
generateClient = do
  let frontEndRoot = "ui/src"
  writeAPIModuleWithSettings mySettings frontEndRoot myBridgeProxy (Proxy :: Proxy Api)
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes
