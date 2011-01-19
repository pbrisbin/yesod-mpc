{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- |
-- Module        : Yesod.Helpers.MPC
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : unstable
-- Portability   : unportable
--
-- A Yesod subsite allowing in-browser controls for MPD.
--
-- Note:
--
-- Since each page load and redirect makes a new connection to MPD (and 
-- I've yet to figure out to prevent this), I recommend you adjust your 
-- settings in @\/etc\/mpd.conf@ to prevent \"MPD Connection timeout\" 
-- errors from appearing on the page during rapid next events.
--
-- > connection_timeout	"15" # was 60
-- > max_connections	"30" # was 10
--
-- seem to work for me.
--
-------------------------------------------------------------------------------
module Yesod.Helpers.MPC 
    ( MPC
    , getMPC
    , YesodMPC(..)
    , MpdConfig(..)
    ) where

import Yesod

import Text.Hamlet
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax hiding (lift)

import qualified Data.Map as M
import qualified Network.MPD as MPD

--import System.IO
--
--debug :: (YesodMPC m) => String -> GHandler s m ()
--debug = liftIO . hPutStrLn stderr

-- | Customize your connection to MPD
data MpdConfig = MpdConfig
    { mpdHost     :: MPD.Host
    , mpdPort     :: MPD.Port
    , mpdPassword :: String
    }

data MPC = MPC

getMPC :: a -> MPC
getMPC = const MPC

class Yesod m => YesodMPC m where
    -- | seconds, default is 10, return 0 to disable
    refreshSpeed :: GHandler s m Int
    refreshSpeed = return 10

    -- | default is Nothing, standard connection
    mpdConfig :: GHandler s m (Maybe MpdConfig)
    mpdConfig = return Nothing

    -- | default is return (), don't authenticate
    authHelper :: GHandler s m ()
    authHelper = return ()

mkYesodSub "MPC" 
    [ ClassP ''YesodMPC [ VarT $ mkName "master" ]
    ] 
    [$parseRoutes|
    /            StatusR GET
    /prev        PrevR   GET
    /pause       PauseR  GET
    /next        NextR   GET
    /play/#Int   PlayR GET
    /delete/#Int DelR  GET
    |]

-- | Wrap MPD.withMPD or MPD.withMPDEx depending on the users mpd 
--   configuration
withMPD :: YesodMPC m => MPD.MPD a -> GHandler MPC m (MPD.Response a)
withMPD f = do
    config <- mpdConfig
    liftIO $ case config of
        Nothing -> MPD.withMPD f
        Just c  -> MPD.withMPDEx (mpdHost c) (mpdPort c) (mpdPassword c) f

-- | This is the main landing page. present now playing info and simple 
--   prev, pause, next controls. todo:s include playlist and library 
--   support, more advanced controls, maybe some album art
getStatusR :: YesodMPC m => GHandler MPC m RepHtml
getStatusR = do
    authHelper
    delay    <- liftM (*1000) refreshSpeed
    toMaster <- getRouteToMaster
    playing  <- getNowPlaying
    playlist <- formattedPlaylist toMaster 10
    controls <- playerControls toMaster
    defaultLayout $ do
        setTitle $ string "MPD"

        -- some sane styling
        -- addCassius... {{{
        addCassius [$cassius|
        .mpc_title
            font-weight:  bold
            font-size:    200%
            padding-left: 10px

        .mpc_controls table
            margin-left:    auto
            margin-right:   auto
            padding:        5px
            padding-top:    20px
            padding-bottom: 20px

        .mpc_playlist table
            margin-left:  auto
            margin-right: auto

        .mpc_playlist th
            border-bottom: solid 1px

        .mpc_playlist td
            padding-left:  5px
            padding-right: 5px

        tr.mpc_current
            font-weight: bold

        td.mpc_playlist_button
            text-align: center
            font-size:  75%
            width:      20px

        .mpc a:link, .mpc a:visited, .mpc a:hover
            outline:         none
            text-decoration: none
        |]
        -- }}}

        -- auto refresh function
        if delay /= 0
            then addJulius [$julius|
                function timedRefresh() {
                    setTimeout("location.reload(true);", %show.delay%);
                }
                |]
            else addJulius [$julius|
                function timedRefrehs() {
                    ; // no-op
                }
                |]

        -- page content
        addHamlet [$hamlet| 
        .mpc
            %h1 MPD 

            ^playing^
            ^playlist^
            ^controls^

            %script
                window.onload = timedRefresh;
            %noscript
                %em note: javascript is required for auto-refresh
        |]

-- | Previous
getPrevR :: YesodMPC m => GHandler MPC m RepHtml
getPrevR = authHelper >> actionRoute MPD.previous

-- | Smart play/pause button
getPauseR :: YesodMPC m => GHandler MPC m RepHtml
getPauseR = authHelper >> getPlayPause >>= actionRoute
    where
        -- | return the correct function give the current state
        getPlayPause :: YesodMPC m => GHandler MPC m (MPD.MPD ())
        getPlayPause = do
            result <- withMPD MPD.status
            case result of
                Right status -> case MPD.stState status of
                    MPD.Playing -> return $ MPD.pause True
                    MPD.Stopped -> return $ MPD.play Nothing
                    MPD.Paused  -> return $ MPD.play Nothing
                -- meh, should probably handle this
                Left _ -> return $ MPD.play Nothing

-- | Next
getNextR :: YesodMPC m => GHandler MPC m RepHtml
getNextR = authHelper >> actionRoute MPD.next

-- | Play a specific song in playlist
getPlayR :: YesodMPC m => Int -> GHandler MPC m RepHtml
getPlayR pid = authHelper >> actionRoute (MPD.playId pid)

getDelR :: YesodMPC m => Int -> GHandler MPC m RepHtml
getDelR pid = authHelper >> actionRoute (MPD.deleteId pid)

-- | Execute any mpd action then redirect back to the main status page
actionRoute :: YesodMPC m => MPD.MPD a -> GHandler MPC m RepHtml
actionRoute f = do
    toMaster <- getRouteToMaster
    _        <- withMPD f
    redirect RedirectTemporary $ toMaster StatusR

-- | Return formatted now playing information
getNowPlaying :: YesodMPC m => GHandler MPC m (Hamlet a)
getNowPlaying = do
    result <- withMPD MPD.currentSong
    state  <- getCurrentState
    case result of
        Left err          -> return [$hamlet| %em $string.show.err$ |]
        Right Nothing     -> return [$hamlet| %em -/-               |]
        Right (Just song) -> return $ formatState state song
    where
        -- output state
        getCurrentState :: YesodMPC m => GHandler MPC m String
        getCurrentState = do
            result <- withMPD MPD.status
            case result of
                Left err     -> return $ show err
                Right status -> case MPD.stState status of
                    MPD.Playing -> return "playing"
                    MPD.Stopped -> return "stopped"
                    MPD.Paused  -> return "paused"

        -- parse a Songs metatdata into a table
        formatState :: String -> MPD.Song -> Hamlet a
        formatState state song = let 
            title  = getTag MPD.Title  song
            artist = getTag MPD.Artist song
            album  = getTag MPD.Album  song
            year   = getTag MPD.Date   song
            in [$hamlet|
            .mpc_nowplaying
                %p
                    %span.mpc_state  $state$
                    \ / 
                    %span.mpc_artist $artist$
                    \ - 
                    %span.mpc_album  $album$
                    \ 
                    %span.mpc_year   ($year$)

                %p
                    %span.mpc_title $title$
            |]

-- | The control links themselves
playerControls :: (YesodMPC m, HamletValue a) => (MPCRoute -> HamletUrl a) -> GHandler MPC m a
playerControls toMaster = return [$hamlet|
    .mpc_controls
        %table
            %tr
                %td
                    %a!href=@toMaster.PrevR@  [ << ]
                %td
                    %a!href=@toMaster.PauseR@ [ || ]
                %td
                    %a!href=@toMaster.NextR@  [ >> ]
    |]

-- | A formatted play list, limited, auto-centered/highlighted on now 
--   playing, and with links to play and remove the entries
formattedPlaylist :: (YesodMPC m, HamletValue a)
                  => (MPCRoute -> HamletUrl a) -- ^ route to master
                  -> Int                       -- ^ limit display
                  -> GHandler MPC m a
formattedPlaylist toMaster limit = do
    -- position and id of currently playing song
    (pos,cid) <- liftM (fromMaybe (0,0)) currentId

    -- length of current playlist
    len <- do 
        -- length of current playlist
        result <- withMPD $ MPD.playlistInfo Nothing
        case result of
            Left _      -> return 0
            Right songs -> return $ length songs

    -- find bounds to show len lines of context
    let (lower,upper) = fixBounds pos len limit

    -- get the limited, auto-centered playlist records
    result <- withMPD $ MPD.playlistInfo (Just (lower, upper))
    case result of
        Left err    -> return [$hamlet| %em $string.show.err$ |]
        Right songs -> return [$hamlet|
            .mpc_playlist
                %table
                    %tr
                        %th #
                        %th Artist
                        %th Title
                        %th Play
                        %th Remove
                    $forall songs song
                        ^formatSong.song^
            |]
            where
                formatSong song = let
                    artist = getTag MPD.Artist song
                    title  = getTag MPD.Title  song
                    pid    = fromMaybe 0 . fmap snd $ MPD.sgIndex song
                    in [$hamlet| 
                    %tr.$clazz.pid$
                        %td $string.show.pid$
                        %td $artist$
                        %td $title$
                        %td.mpc_playlist_button
                            %a!href=@toMaster.PlayR.pid@ |>
                        %td.mpc_playlist_button
                            %a!href=@toMaster.DelR.pid@  X
                    |]
                    where
                        clazz x = if x == cid
                            then string "mpc_current"
                            else string "mpc_not_current"

-- | Show playlist context with now playing centered but ensure we don't 
--   send invalide upper and lower bounds to the request
fixBounds :: Int -- ^ pos of currently playing track
          -> Int -- ^ length of current playlist
          -> Int -- ^ how many lines of context to show (+/- 1)
          -> (Int, Int)
fixBounds pos len limit = let
    lower = pos - limit `div` 2
    upper = pos + limit `div` 2
    in if lower < 0 && upper > len
        then (0, len)
        else if lower < 0
            then fixBounds (limit `div` 2) len limit
            else if upper > len
                then fixBounds (pos - (upper - len)) len limit
                else (lower, upper)

-- | Return maybe the id of the currently playing song
currentId :: YesodMPC m => GHandler MPC m (Maybe (Int,Int))
currentId = do
    result <- withMPD MPD.currentSong
    case result of
        Left _            -> return Nothing
        Right Nothing     -> return Nothing
        Right (Just song) -> return $ MPD.sgIndex song

-- | Get the first instance of the given tag in the the passed song, 
--   return "N/A" if it's not found
getTag :: MPD.Metadata -> MPD.Song -> Html
getTag tag = string . head . M.findWithDefault ["N/A"] tag . MPD.sgTags
