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

instance Yesod MpcTest where 
    approot _ = ""
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            widget
        hamletToRepHtml [$hamlet|
            !!!
            %html!lang="en"
                %head
                    %script!src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js"
                    ^pageHead.pc^
                    %title $pageTitle.pc$
                %body
                    ^pageBody.pc^
            |]


-- | Make your site an instance of YesodMPC using all default values
instance YesodMPC MpcTest

main :: IO ()
main = putStrLn "Loaded" >> toWaiApp MpcTest >>= run 3000
