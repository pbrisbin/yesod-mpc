{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
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

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax hiding (lift)

import qualified Data.Map as M
import qualified Network.MPD as MPD

--import System.IO
--
--debug :: (YesodMPC m) => String -> GHandler s m ()
--debug = liftIO . hPutStrLn stderr

-- | Represents now playing state, see 'nowPlaying' for how this is 
--   constructed when needed.
data NowPlaying = NowPlaying
    { npState  :: String
    , npTitle  :: String
    , npArtist :: String
    , npAlbum  :: String
    , npYear   :: String
    , npPos    :: Int    -- ^ position in playlist
    , npId     :: Int    -- ^ playlist id
    , npCur    :: String -- ^ time elapsed, MM:SS
    , npTot    :: String -- ^ song length, MM:SS
    }

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
    -- | seconds, default is 1, return 0 to disable
    refreshSpeed :: GHandler s m Int
    refreshSpeed = return 1

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
    /play/#Int   PlayR   GET
    /delete/#Int DelR    GET

    /statecheck.xml CheckR GET
    |]

-- | Wrap MPD.withMPD or MPD.withMPDEx depending on the users mpd 
--   configuration
withMPD :: YesodMPC m => MPD.MPD a -> GHandler MPC m (MPD.Response a)
withMPD f = do
    config <- mpdConfig
    liftIO $ case config of
        Nothing -> MPD.withMPD f
        Just c  -> MPD.withMPDEx (mpdHost c) (mpdPort c) (mpdPassword c) f

-- | Return now playing info or nothing
nowPlaying :: YesodMPC m => GHandler MPC m (Maybe NowPlaying)
nowPlaying = do
    songResp  <- withMPD MPD.currentSong
    stateResp <- withMPD MPD.status
    case (songResp,stateResp) of
        (Right (Just song), Right state) -> return $ 
            Just NowPlaying
                { npTitle  = getTag MPD.Title song
                , npArtist = getTag MPD.Artist song
                , npAlbum  = getTag MPD.Album song
                , npYear   = getTag MPD.Date song
                , npState  = case MPD.stState state of
                    MPD.Playing -> "playing"
                    MPD.Paused  -> "paused"
                    MPD.Stopped -> "stopped"
                , npPos    = fromMaybe 0 . fmap fst $ MPD.sgIndex song
                , npId     = fromMaybe 0 . fmap snd $ MPD.sgIndex song
                , npCur    = format . round .               fst $ MPD.stTime state
                , npTot    = format . round . fromInteger . snd $ MPD.stTime state
                }
        -- todo: make this an Either and return the error(s)
        _ -> return Nothing
    where
        -- convert seconds to MM:SS handling both Double and Integer types
        format n = let minutes = n `div` 60
                       left    = n `mod` 60 in pad minutes ++ ":" ++ pad left
        pad = (\s -> if length s == 1 then '0': s else s) . show

-- Routes {{{
-- | This is the main landing page. present now playing info and simple 
--   prev, pause, next controls. todo:s include playlist and library 
--   support, more advanced controls, maybe some album art
getStatusR :: YesodMPC m => GHandler MPC m RepHtml
getStatusR = do
    authHelper
    toMaster <- getRouteToMaster
    delay    <- liftM (*1000) refreshSpeed
    defaultLayout $ do
        setTitle $ string "MPD"

        -- ajax-powerd refresh {{{
        addJulius [$julius|
            var xmlhttp = new XMLHttpRequest();

            // handle the callback
            xmlhttp.onreadystatechange = function()
            {
                if (xmlhttp.readyState == 4 && xmlhttp.status == 200)
                {
                    xmlDoc  = xmlhttp.responseXML;
                    xStatus = xmlHelper(xmlDoc, "status");
                    
                    if (xStatus == "OK")
                    {
                        xPos = xmlHelper(xmlDoc, "pos");
                        xId  = xmlHelper(xmlDoc, "id");

                        curPos = docHelper(false, "mpc_pos", "");
                        curId  = docHelper(false, "mpc_id",  "");

                        if (xPos != curPos || xId != curId) {
                            // track changed, reload
                            location.reload(true);
                            return;
                        }

                        // update state if needed
                        xState   = xmlHelper(xmlDoc, "state");
                        curState = docHelper(false, "mpc_state", "");

                        if (xState != curState) {
                            docHelper(true, "mpc_state", xState);
                        }

                        // always update time
                        xCur = xmlHelper(xmlDoc, "cur");
                        docHelper(true, "mpc_cur", xCur)
                    }
                }
            }

            /* return the content of an xml tag */
            function xmlHelper(_xmlDoc, _tag) {
                return _xmlDoc.getElementsByTagName(_tag)[0].childNodes[0].nodeValue;
            }

            /* set and get a document tag's inner value */
            function docHelper(_set, _id, _value) {
                if (_set) {
                    document.getElementById(_id).innerHTML = _value;
                }
                return document.getElementById(_id).innerHTML;
            }

            /* ask the server for updated now playing info */
            function getNowPlaying() {
                xmlhttp.open("GET", "@toMaster.CheckR@", true);
                xmlhttp.send();
                timedRefresh();
            }

            /* setup the refresh if delay is non-zero */
            function timedRefresh() {
                var delay = %show.delay%;

                if (delay != 0)
                    setTimeout("getNowPlaying();", delay);
            }
            |]
        -- }}}

        -- page content
        addHamlet [$hamlet| %h1 MPD |]
        nowPlayingWidget
        playListWidget 10
        playerControlsWidget

        addHamlet [$hamlet|
            %script window.onload = timedRefresh;
            %noscript
                note: javascript is required for screen updates.
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

-- | Return now playing information as xml for an AJAX request. Note 
--   that this request is not authenticated even if you set an 
--   'authHelper'.
getCheckR :: YesodMPC m => GHandler MPC m RepXml
getCheckR = do
    result <- nowPlaying
    fmap RepXml . hamletToContent $ case result of
        Nothing -> [$xhamlet|
            \<?xml version="1.0" encoding="utf-8"?>
            %xml
                %status ERR
                %error  MPD threw an error
            |]
        Just np -> [$xhamlet|
            \<?xml version="1.0" encoding="utf-8"?>
            %xml
                %status OK
                %state  $npState.np$
                %title  $npTitle.np$
                %artist $npArtist.np$
                %album  $npAlbum.np$
                %year   $npYear.np$
                %pos    $show.npPos.np$
                %id     $show.npId.np$
                %cur    $npCur.np$
                %tot    $npTot.np$
            |]
-- }}}

-- Widgets {{{
nowPlayingWidget :: YesodMPC m => GWidget MPC m ()
nowPlayingWidget = do
    addCassius [$cassius|
        #mpc_title
            font-weight:  bold
            font-size:    200%
            padding-left: 10px

        .mpc_elapsed
            float: right
        |]

    result <- liftHandler nowPlaying
    case result of
        Nothing -> addHamlet [$hamlet| %em N/A |]
        Just np -> addHamlet [$hamlet|
            .mpc_nowplaying
                %p
                    %span#mpc_pos!style="display: none;" $show.npPos.np$
                    %span#mpc_id!style="display: none;"  $show.npId.np$

                    %span#mpc_state  $npState.np$
                    \ / 
                    %span#mpc_artist $npArtist.np$
                    \ - 
                    %span#mpc_album  $npAlbum.np$
                    \ (
                    %span#mpc_year   $npYear.np$
                    )

                    %span.mpc_elapsed
                        %span#mpc_cur    $npCur.np$
                        \ / 
                        %span#mpc_tot    $npTot.np$
                %p
                    %span#mpc_title $npTitle.np$
            |]

-- | The control links themselves
playerControlsWidget :: YesodMPC m => GWidget MPC m ()
playerControlsWidget = do
    addCassius [$cassius|
        .mpc_controls table
            margin-left:    auto
            margin-right:   auto
            padding:        5px
            padding-top:    20px
            padding-bottom: 20px

        .mpc_controls a:link, .mpc_controls a:visited, .mpc_controls a:hover
            outline:         none
            text-decoration: none
        |]

    toMaster <- liftHandler getRouteToMaster
    addHamlet [$hamlet|
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
playListWidget :: YesodMPC m
                  => Int -- ^ limit display
                  -> GWidget MPC m ()
playListWidget limit = do
    addCassius [$cassius|
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

        .mpc_playlist a:link, .mpc_playlist a:visited, .mpc_playlist a:hover
            outline:         none
            text-decoration: none
        |]

    toMaster  <- liftHandler getRouteToMaster
    (pos,cid) <- liftM (fromMaybe (0,0)) $ liftHandler currentId
    result    <- liftHandler . withMPD $ MPD.playlistInfo Nothing

    let len = case result of
            Left _      -> 0
            Right songs -> length songs

    -- find bounds to show len lines of context
    let (lower,upper) = fixBounds pos len limit

    -- get the limited, auto-centered playlist records
    result' <- liftHandler . withMPD $ MPD.playlistInfo (Just (lower, upper))

    case result' of
        Left err    -> addHamlet [$hamlet| %em $string.show.err$ |]
        Right songs -> addHamlet [$hamlet|
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

                clazz x = if x == cid
                    then string "mpc_current"
                    else string "mpc_not_current"
-- }}}

-- Helpers {{{
-- | Show playlist context with now playing centered but ensure we don't 
--   send invalide upper and lower bounds to the request
fixBounds :: Int -- ^ pos of currently playing track
          -> Int -- ^ length of current playlist
          -> Int -- ^ how many lines of context to show
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
getTag :: MPD.Metadata -> MPD.Song -> String
getTag tag = head . M.findWithDefault ["N/A"] tag . MPD.sgTags
-- }}
