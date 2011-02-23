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
    ( 
    -- * Usage
    -- $usage
      YesodMPC(..)
    , MpdConfig(..)
    -- * Subsite
    -- $subsite
    , MPC
    , MPCRoute(..)
    , getMPC
    -- * Widgets
    -- $widgets
    , progressBarWidget
    , nowPlayingWidget
    , playListWidget
    , playerControlsWidget
    -- * Now Playing
    -- $now_playing
    , NowPlaying(..)
    , nowPlaying
    ) where

import Yesod

import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax hiding (lift)

import qualified Network.MPD as MPD

--import System.IO
--
--debug :: (YesodMPC m) => String -> GHandler s m ()
--debug = liftIO . hPutStrLn stderr

-- Documentation {{{

-- $usage
--
-- Instantiate your app with the YesodMPC class. All constructors are 
-- optional.
--
-- There are varying levels difficulty in how you use this module 
-- otherwise:
--
-- * Define a subsite route to a nice out-of-the-box display and control
--   page.
--
-- * Use the individual widgets anywhere in your site's existing pages.
--
-- * Use the core 'NowPlaying' object to do whatever you want.
--

-- $subsite
-- 
-- Ex:
--
-- > mkYesod "YourApp" [$parseRoutes| 
-- > / RootR GET
-- > /...
-- > /mpc MpcR MPC getMPC 
-- > |]
--

-- $widgets
--
-- Ex:
--
-- > getRootR :: Handler RepHtml
-- > getRootR = defaultLayout $ do
-- >     addJulius [$julius|
-- >         // ajax code here
-- >         |]
-- >
-- >     nowPlayingWidget
-- >     progressBarWidget
-- >
-- >     addHamlet [$hamlet| ...
--
-- Note: 
--
-- Each of these widgets contains javascript to update individual 
-- tags. The main subsite uses AJAX to achieve screen updates by 
-- requesting updated now playing info via JSON at its own url.
--
-- Widgets which offer controls require you to define a subsite route 
-- and pass it as an argument.
--

-- $now_playing
--
-- Ex:
--
-- > getRootR :: Handler RepHtml
-- > getRootR = defaultLayout $ do
-- >     mnp <- nowPlaying
-- >     case mnp of
-- >         Nothing -> return ()
-- >         Just np -> do
-- >             addJulius [$julius|
-- >                 // ajax code and update functions
-- >                 |]
-- >             addHamlet [$hamlet|
-- >                 %p currently playing: $npTitle.np$
-- >                 |]
-- >
-- >     addHamlet [$hamlet| ...
--

-- }}}

-- | Represents now playing state, see 'nowPlaying' for how this is 
--   constructed when needed.
data NowPlaying = NowPlaying
    { npState  :: String
    , npTitle  :: String
    , npArtist :: String
    , npAlbum  :: String
    , npYear   :: String
    , npPos    :: Int          -- ^ position in playlist
    , npId     :: Int          -- ^ playlist id
    , npCur    :: String       -- ^ time elapsed, MM:SS
    , npTot    :: String       -- ^ song length, MM:SS
    , npProg   :: Double       -- ^ percentage elapsed for convenience
    , npCover  :: Maybe String -- ^ url to cover image if setup
    }

-- | Customize your connection to MPD
data MpdConfig = MpdConfig
    { mpdHost     :: MPD.Host -- ^ String
    , mpdPort     :: MPD.Port -- ^ Int
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

    -- | (Artist,Album) -> Maybe Cover url: default is Nothing
    albumArtHelper :: (String,String) -> GHandler s m (Maybe String)
    albumArtHelper = return . const Nothing

    -- | In case you serve your own jQuery, default is hosted by google
    jQueryUrl :: GHandler s m String
    jQueryUrl = return "http://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js"

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
    |]

-- | Wrap MPD.withMPD or MPD.withMPDEx depending on the users mpd 
--   configuration
withMPD :: YesodMPC m => MPD.MPD a -> GHandler s m (MPD.Response a)
withMPD f = do
    config <- mpdConfig
    liftIO $ case config of
        Nothing -> MPD.withMPD f
        Just c  -> MPD.withMPDEx (mpdHost c) (mpdPort c) (mpdPassword c) f

-- | Return now playing info or nothing
nowPlaying :: YesodMPC m => GHandler s m (Maybe NowPlaying)
nowPlaying = do
    songResp  <- withMPD MPD.currentSong
    stateResp <- withMPD MPD.status
    case (songResp,stateResp) of
        (Right (Just song), Right state) -> do
            let artist = getTag MPD.Artist song
            let album  = getTag MPD.Album  song
            coverurl <- albumArtHelper (artist,album)
            return $ Just NowPlaying
                { npTitle  = getTag MPD.Title song
                , npArtist = artist
                , npAlbum  = album
                , npYear   = getTag MPD.Date song
                , npState  = case MPD.stState state of
                    MPD.Playing -> "playing"
                    MPD.Paused  -> "paused"
                    MPD.Stopped -> "stopped"
                , npPos    = fromMaybe (-1) . fmap fst $ MPD.sgIndex song
                , npId     = fromMaybe (-1) . fmap snd $ MPD.sgIndex song
                , npCur    = format . round . fst $ MPD.stTime state
                , npTot    = format .         snd $ MPD.stTime state
                , npProg   = progress $ MPD.stTime state
                , npCover  = coverurl
                } 
        -- todo: make this an Either and return the error(s)
        _ -> return Nothing
    where
        -- convert seconds to MM:SS
        format = (\(q,r) -> pad q ++ ":" ++ pad r) . flip divMod 60
        pad    = (\s -> if length s == 1 then '0':s else s) . show

        -- convert stTime into percentage
        progress :: (Double, MPD.Seconds) -> Double
        progress (d,i) = (*100) (d / realToFrac i)

-- Routes {{{
getStatusR :: YesodMPC m => GHandler MPC m RepHtmlJson
getStatusR = do
    authHelper
    toMaster <- getRouteToMaster
    delay    <- fmap (*1000) refreshSpeed
    jQuery   <- jQueryUrl
    nowPlaying >>= \mnp ->
        defaultLayoutJson (do
            let lim = 10
            setTitle $ string $ "MPD" <> (maybe "" npTitle mnp) -- todo: provide updateTitle();
            
            -- ajax refresh {{{
            addJulius [$julius|
                function getNowPlaying() {
                    var delay = %show.delay%;

                    if (delay != 0) {
                        $.getJSON(window.location.href, {}, function(o) {
                            if (o.status == "OK") {
                                /* track's changed, refresh page */
                                if (document.getElementById("mpc_pos").innerHTML != o.pos ||
                                    document.getElementById("mpc_id").innerHTML  != o.id) {
                                    location.reload(true);
                                    return;
                                };

                                if (typeof updateState    == 'function') { updateState(o.state);       }
                                if (typeof updateCur      == 'function') { updateCur(o.cur);           }
                                if (typeof updateProgress == 'function') { updateProgress(o.progress); }
                            }
                        });

                        setTimeout("getNowPlaying();", delay);
                    }
                }
            |]
            -- }}}

            -- page content; java script is added end of body for faster 
            -- page load
            [$hamlet| 
                %h1 MPD
                ^nowPlayingWidget^
                ^(playListWidget.toMaster).lim^
                ^playerControlsWidget.toMaster^
                ^progressBarWidget^

                %script!src=$jQuery$
                %script $$(function() { getNowPlaying(); });
                %noscript
                    %em javascript is required for screenupdates
                |]) (json mnp)
    where
        x <> "" = x
        x <> y  = x ++ " - " ++ y

        json mnp = case mnp of
            Just np -> jsonMap
                [ ("status"  , jsonScalar $ "OK"  )
                , ("state"   , jsonScalar $ npState np               )
                , ("title"   , jsonScalar $ npTitle np               )
                , ("artist"  , jsonScalar $ npArtist np              )
                , ("album"   , jsonScalar $ npAlbum np               )
                , ("year"    , jsonScalar $ npYear np                )
                , ("pos"     , jsonScalar . show $ npPos np          )
                , ("id"      , jsonScalar . show $ npId np           )
                , ("cur"     , jsonScalar $ npCur np                 )
                , ("tot"     , jsonScalar $ npTot np                 )
                , ("progress", jsonScalar . show $ npProg np         )
                , ("coverurl", jsonScalar . fromMaybe "" $ npCover np)
                ]
                
            _ -> jsonMap
                [ ("status", jsonScalar $ "ERR"               )
                , ("error" , jsonScalar $ "MPD threw an error")
                ]

-- | Previous
getPrevR :: YesodMPC m => GHandler MPC m RepHtml
getPrevR = authHelper >> actionRoute MPD.previous

-- | Smart play/pause button
getPauseR :: YesodMPC m => GHandler MPC m RepHtml
getPauseR = authHelper >> getPlayPause >>= actionRoute
    where
        -- | return the correct function given the current state
        getPlayPause :: YesodMPC m => GHandler MPC m (MPD.MPD ())
        getPlayPause = withMPD MPD.status >>= either fromErr fromRes
            where
                fromErr _      = return $ MPD.play Nothing
                fromRes status = return $ case MPD.stState status of
                    MPD.Playing -> MPD.pause True
                    MPD.Stopped -> MPD.play Nothing
                    MPD.Paused  -> MPD.play Nothing

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

-- }}}

-- Widgets {{{
-- | Show now playing info.
nowPlayingWidget :: YesodMPC m => GWidget s m ()
nowPlayingWidget = do
    addJulius [$julius|
        /* generic */
        function updateTagById(_id, _newValue) {
            var e = document.getElementById(_id);
            if (e.innerHTML != _newValue) { e.innerHTML = _newValue; }
        }

        /* provide a function for each tag */
        function updateState( _val) { updateTagById("mpc_state" , _val); }
        function updatePos(   _val) { updateTagById("mpc_pos"   , _val); }
        function updateId(    _val) { updateTagById("mpc_id"    , _val); }
        function updateTitle( _val) { updateTagById("mpc_title" , _val); }
        function updateArtist(_val) { updateTagById("mpc_artist", _val); }
        function updateAlbum( _val) { updateTagById("mpc_album" , _val); }
        function updateYear(  _val) { updateTagById("mpc_year"  , _val); }
        function updateCur(   _val) { updateTagById("mpc_cur"   , _val); }
        function updateTot(   _val) { updateTagById("mpc_tot"   , _val); }

        /* update the cover image */
        function updateCover( _val) {
            var e = document.getElementById("mpc_cover");
            if (e.src != _val) { e.src = _val; }
        }
        |]

    addCassius [$cassius|
        #mpc_cover
            float:         left
            height:        75px
            width:         75px
            margin-right:  5px

        #mpc_title
            font-weight:  bold
            font-size:    200%
            padding-left: 10px

        .mpc_elapsed
            float: right
        |]

    mnp <- liftHandler nowPlaying
    [$hamlet|
        $maybe mnp np
            .mpc_nowplaying
                $maybe npCover.np cover
                    %img#mpc_cover!src=$cover$

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

-- | Prev, Play/Pause, Next
playerControlsWidget :: YesodMPC m 
                     => (MPCRoute -> Route m) -- ^ your subsite route
                     -> GWidget s m ()
playerControlsWidget mpcR = do
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

    addHamlet [$hamlet|
        .mpc_controls
            %table
                %tr
                    %td
                        %a!href=@mpcR.PrevR@  [ &lt;&lt; ]
                    %td
                        %a!href=@mpcR.PauseR@ [ || ]
                    %td
                        %a!href=@mpcR.NextR@  [ &gt;&gt; ]
        |]

-- | A formatted play list, limited, auto-centered/highlighted on now 
--   playing.
playListWidget :: YesodMPC m
                  => (MPCRoute -> Route m) -- ^ your subsite route
                  -> Int                   -- ^ limit display
                  -> GWidget s m ()
playListWidget mpcR limit = do
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

        .mpc_playlist a:link, .mpc_playlist a:visited, .mpc_playlist a:hover
            outline:         none
            text-decoration: none
        |]

    mnp <- liftHandler nowPlaying
    let pos = fromMaybe (-1) $ fmap npPos mnp
    let cid = fromMaybe (-1) $ fmap npId  mnp

    result <- liftHandler . withMPD $ MPD.playlistInfo Nothing
    let len = either (const 0) length result

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
                        %th Album
                        %th Title

                    $forall songs song
                        ^formatSong.song^
            |]
            where
                formatSong song = let
                    artist = getTag MPD.Artist song
                    album  = getTag MPD.Album  song
                    title  = getTag MPD.Title  song
                    pid    = fromMaybe 0 . fmap snd $ MPD.sgIndex song
                    in [$hamlet| 
                        %tr.$clazz.pid$
                            %td 
                                %a!href=@mpcR.PlayR.pid@ $string.show.pid$
                            %td $artist$
                            %td $album$
                            %td $title$
                        |]

                clazz x = if x == cid
                    then string "mpc_current"
                    else string "mpc_not_current"

-- | Show a \"progress bar\" by changing the width of a div element.
progressBarWidget :: YesodMPC m => GWidget s m ()
progressBarWidget = do
    addJulius [$julius|
        function updateProgress(_int) {
            document.getElementById("mpc_progress_inner").style.width = _int + "%%";
        }
        |]

    addCassius [$cassius|
        #mpc_progress_inner
            width:         0px
            border-bottom: 2px solid
        |]

    addHamlet [$hamlet| 
        #mpc_progress_outer
            #mpc_progress_inner &nbsp;
        |]
-- }}}

-- Helpers {{{
-- | Show playlist context with now playing centered but ensure we don't 
--   send invalid upper and lower bounds to the request
fixBounds :: Int -- ^ pos of currently playing track
          -> Int -- ^ length of current playlist
          -> Int -- ^ how many lines of context to show
          -> (Int, Int)
fixBounds pos len limit = let
    lower = pos - limit `div` 2
    upper = pos + limit `div` 2
    in go lower upper
    where
        go l u
            | l < 0 && u > len = (0,len)
            | l < 0            = fixBounds (limit `div` 2)   len limit
            | u > len          = fixBounds (pos - (u - len)) len limit
            | otherwise        = (l,u)

-- | Get the first instance of the given tag in the the passed song, 
--   return "N/A" if it's not found
getTag :: MPD.Metadata -> MPD.Song -> String
getTag tag = head . fromMaybe ["N/A"] . MPD.sgGet tag

-- }}
