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

-- | Make your site an instance of YesodMPC using all default values
instance YesodMPC MpcTest

main :: IO ()
--main = putStrLn "Loaded" >> toWaiApp MpcTest >>= run 3000
main = putStrLn "Loaded" >> toWaiApp MpcTest >>= run 8080
