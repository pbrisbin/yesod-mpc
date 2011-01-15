{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
--
-- pbrisbin 2010
--
-- This is a single-file test application which shows how to use the
-- mpc module as part of your site
--
module Test where

import Yesod.Helpers.MPC

import Yesod
import Network.Wai.Handler.SimpleServer

data CommentTest = CommentTest
type Handler = GHandler CommentTest CommentTest

-- | Make sure you allow the POST route on any routes where comments
--   will be entered. It can just link to GET (see 'postRootR')
mkYesod "CommentTest" [$parseRoutes| /mpc MpcR MPC getMPC |]

-- | Main app definition
instance Yesod CommentTest where approot _ = ""

-- | Make your site an instance of YesodMPC
instance YesodMPC CommentTest where
    refreshSpeed = return 10
    mpdConfig    = return Nothing
    authHelper   = return ()

-- | Run the app
main :: IO ()
main = putStrLn "Loaded" >> withCommentTest (run 3000)

withCommentTest :: (Application -> IO a) -> IO a
withCommentTest f = toWaiApp CommentTest >>= f
