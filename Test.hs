{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
-- pbrisbin 2010
--
-- How to use Yesod.Helpers.MPC
--
module Test where

import Yesod
import Yesod.Helpers.MPC
import Network.Wai.Handler.Warp (run)
import qualified Network.MPD as MPD

data MpcTest = MpcTest

mkYesod "MpcTest" [parseRoutes| / MpcR MPC getMPC |]

instance Yesod MpcTest where approot _ = ""

instance YesodMPC MpcTest

main :: IO ()
main = putStrLn "Loaded" >> toWaiApp MpcTest >>= run 3000
