{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE TypeFamilies #-}
--
-- pbrisbin 2010
--
-- How to use Yesod.Helpers.MPC
--
module Test where

import Yesod
import Yesod.Helpers.MPC
import Network.Wai.Handler.SimpleServer (run)

data MpcTest = MpcTest

-- | Have an mpc route
mkYesod "MpcTest" [$parseRoutes| /mpc MpcR MPC getMPC |]

instance Yesod MpcTest where approot _ = ""

-- | Make your site an instance of YesodMPC
instance YesodMPC MpcTest where
    refreshSpeed = return 10      -- ^ page refreshes every 10 seconds
    mpdConfig    = return Nothing -- ^ use default connection
    authHelper   = return ()      -- ^ don't authenticate

main :: IO ()
main = putStrLn "Loaded" >> toWaiApp MpcTest >>= run 3000
