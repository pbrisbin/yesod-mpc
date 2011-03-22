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
-------------------------------------------------------------------------------
module Yesod.Helpers.MPC 
    ( 
    -- * Usage
      YesodMPC(..)
    , MpdConfig(..)
    -- * Subsite
    , MPC
    , MPCRoute(..)
    , getMPC
    -- * MPD interface
    , withMPD
    , NowPlaying(..)
    , nowPlaying
    , PlayListItem(..)
    , PlayList
    , playList
    ) where

import Yesod
import Text.Blaze (toHtml)

import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax hiding (lift)

import qualified Network.MPD as MPD

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

data PlayListItem url = PlayListItem
    { plNo      :: Int
    , plArtist  :: String
    , plAlbum   :: String
    , plTitle   :: String
    , plPlaying :: Bool
    , plRoute   :: url
    }

type PlayList url = [PlayListItem url]

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
    jQueryUrl = return "http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"

mkYesodSub "MPC" 
    [ ClassP ''YesodMPC [ VarT $ mkName "master" ] ] 
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

-- | Return the current playlist context or the empty list
playList :: YesodMPC m => NowPlaying -> GHandler s m (PlayList MPCRoute)
playList np = do
    -- determine overall playlist length
    len <- return . either (const 0) length =<< withMPD (MPD.playlistInfo Nothing)
    let bounds = Just $ fixBounds (npPos np) len 10

    result' <- withMPD $ MPD.playlistInfo bounds
    case result' of
        Left _      -> return []
        Right songs -> return $ map (itemFromSong $ npId np) songs

    where
        itemFromSong :: Int -> MPD.Song -> PlayListItem MPCRoute
        itemFromSong cid song = let num = fromMaybe 0 . fmap snd $ MPD.sgIndex song
            in PlayListItem
                { plArtist  = getTag MPD.Artist song
                , plAlbum   = getTag MPD.Album  song
                , plTitle   = getTag MPD.Title  song
                , plNo      = num
                , plPlaying = cid == num
                , plRoute   = PlayR num
                }

-- | Main page
getStatusR :: YesodMPC m => GHandler MPC m RepHtmlJson
getStatusR = do
    authHelper
    mnp <- nowPlaying
    mpl <- playList =<? mnp

    tm <- getRouteToMaster
    r  <- getUrlRender

    -- reply with main html view or json updates
    defaultLayoutJson (htmlRep mnp mpl) (jsonRep tm r mnp mpl)
        
    where
        htmlRep (Just np) (Just pl) = do
            jQuery   <- lift jQueryUrl
            toMaster <- lift getRouteToMaster
            delay    <- lift $ fmap (*1000) refreshSpeed
           
            setTitle $ toHtml ("MPD - " ++ npTitle np)
            
            -- javascript {{{
            addJulius [$julius|
                // add all rows of a playlist item as a table
                function addRows(_playlist) {
                    var ret = "";

                    for (var i in _playlist) {
                        var item = _playlist[i];

                        // todo: fix Yesod.Json to send booleans
                        ret += item.playing == "true" ? '<tr class="mpc_current">' : '<tr>';

                        ret += '<td><a href="' + item.route + '">' + item.no + "</a></td>"
                            +  "<td>" + item.artist + "</td>"
                            +  "<td>" + item.album  + "</td>"
                            +  "<td>" + item.title  + "</td>"

                        ret += "</tr>"
                    }

                    return ret;
                }

                function getNowPlaying() {
                    var delay = #{show delay}; // server-set variable

                    if (delay != 0) {
                        $.getJSON(window.location.href, {}, function(o) {
                            if (o.status == "OK") {
                                if (o.pos == $("#mpc_pos").text() && o.id  == $("#mpc_id").text()) {
                                    // same track only update a few indicators
                                    if (o.state != $("#mpc_state").text()) $("#mpc_state").text(o.state);
                                    if (o.tot   != $("#mpc_tot").text())   $("#mpc_tot").text(o.tot);
                                    if (o.cur   != $("#mpc_cur").text())   $("#mpc_cur").text(o.cur);

                                    // update progress
                                    $("#mpc_progress_inner").css( { width: o.progress + "%" } );
                                }
                                else {
                                    // new track, update everything
                                    if (o.state  != $("#mpc_state").text())  $("#mpc_state").text(o.state);
                                    if (o.tot    != $("#mpc_tot").text())    $("#mpc_tot").text(o.tot);
                                    if (o.cur    != $("#mpc_cur").text())    $("#mpc_cur").text(o.cur);
                                    if (o.artist != $("#mpc_artist").text()) $("#mpc_artist").text(o.artist);
                                    if (o.album  != $("#mpc_album").text())  $("#mpc_album").text(o.album);
                                    if (o.year   != $("#mpc_year").text())   $("#mpc_year").text(o.year);

                                    if (o.title != $("#mpc_title").text()) {
                                        $("#mpc_title").text(o.title);
                                        document.title = "MPD - " + o.title;
                                    }

                                    // update cover image
                                    if (o.coverurl != $("#mpc_cover").attr("src")) $("#mpc_cover").attr("src", o.coverurl);

                                    // update progress
                                    $("#mpc_progress_inner").css( { width: o.progress + "%" } );

                                    // update playlist
                                    $(".mpc_playlist").html("<table>"
                                        + "<tr><th>No</th><th>Artist</th><th>Album</th><th>Title</th></tr>"
                                        + addRows(o.playlist)
                                        + "</table>");
                                }
                            }
                        });

                        setTimeout("getNowPlaying();", delay);
                    }
                }
            |]
            -- }}}
            
            -- css {{{
            addCassius [$cassius|
                #mpc_cover
                    float:        left
                    height:       72px
                    width:        72px
                    margin-right: 5px
                #mpc_title
                    font-weight:  bold
                    font-size:    200%
                    padding-left: 10px
                .mpc_elapsed
                    float: right
                .mpc_playlist table
                    margin-left:  auto
                    margin-right: auto
                .mpc_playlist th
                    border-bottom: solid 1px
                .mpc_playlist td
                    padding: 2px 5px;
                tr.mpc_current
                    font-weight: bold
                .mpc_playlist a:link, .mpc_playlist a:visited, .mpc_playlist a:hover
                    outline:         none
                    text-decoration: none
                .mpc_controls table
                    margin:  0px auto;
                    padding: 5px 20px;
                .mpc_controls a:link, .mpc_controls a:visited, .mpc_controls a:hover
                    outline:         none
                    text-decoration: none
                #mpc_progress_inner
                    width:         0px
                    border-bottom: 3px solid
                |]
            -- }}}

            -- html {{{
            [$hamlet|
                <h1>MPD

                \<!-- now playing {{{ -->
                <div .mpc_nowplaying>
                    $maybe cover <- npCover np
                        <img #mpc_cover src="#{cover}">

                    <p>
                        <span #mpc_pos style="display: none;">#{show $ npPos np}
                        <span #mpc_id style="display: none;">#{show $ npId np}

                        <span #mpc_state>#{npState np}
                        <span #mpc_sep> / 
                        <span #mpc_artist>#{npArtist np}
                        <span #mpc_sep> - 
                        <span #mpc_album>#{npAlbum np}
                        <span #mpc_paren> (
                        <span #mpc_year>#{npYear np}
                        <span #mpc_paren>)

                        <span .mpc_elapsed>
                            <span #mpc_cur>#{npCur np}
                            <span #mpc_sep> / 
                            <span #mpc_tot>#{npTot np}

                    <p>
                        <span #mpc_title>#{npTitle np}

                \<!-- end now playing }}} -->

                \<!-- playlist {{{ -->
                <div .mpc_playlist>
                    <table>
                        <tr>
                            <th>No
                            <th>Artist
                            <th>Album
                            <th>Title

                        $forall item <- pl
                            $if plPlaying item
                                <tr.mpc_current>
                                    <td>
                                        <a href="@{toMaster $ plRoute item}">#{show $ plNo item}
                                    <td>#{plArtist item}
                                    <td>#{plAlbum item}
                                    <Td>#{plTitle item}
                            $else
                                <tr>
                                    <td>
                                        <a href="@{toMaster $ plRoute item}">#{show $ plNo item}
                                    <td>#{plArtist item}
                                    <td>#{plAlbum item}
                                    <Td>#{plTitle item}

                \<!-- end playlist }}} -->

                \<!-- player controls {{{ -->
                <div .mpc_controls>
                    <table>
                        <tr>
                            <td>
                                <a #mpc_prev href="@{toMaster PrevR}">[ &lt;&lt; ]
                            <td>
                                <a #mpc_pp href="@{toMaster PauseR}">[ || ]
                            <td>
                                <a #mpc_next href="@{toMaster NextR}">[ &gt;&gt; ]

                \<!-- end player controls }}} -->

                \<!-- progress bar {{{ -->
                <div #mpc_progress_outer>
                    <div #mpc_progress_inner>&nbsp;

                \<!-- end progress bar }}} -->

                <script src="#{jQuery}">
                <script>$(function() { getNowPlaying(); });

                |]
                -- }}}

        htmlRep _  _ = do
            setTitle $ toHtml "MPD"
            [$hamlet|
                <h1>MPD
                <div .mpc_error>
                    <p>
                        <em>Unable to determine now playing info.
                |]

        jsonRep tm r (Just np) (Just pl) = jsonMap
            [ ("status"  , jsonScalar $ "OK"                     )
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
            , ("playlist", jsonList $ map (jsonItem tm r) pl     )
            ]
                
        jsonRep _ _ _ _ = jsonMap
            [ ("status", jsonScalar $ "ERR"               )
            , ("error" , jsonScalar $ "MPD threw an error")
            ]

        jsonItem tm r item = jsonMap
            [ ( "no"     , jsonScalar . show $ plNo item                          )
            , ( "artist" , jsonScalar $ plArtist item                             )
            , ( "album"  , jsonScalar $ plAlbum item                              )
            , ( "title"  , jsonScalar $ plTitle item                              )
            , ( "playing", jsonScalar $ if plPlaying item then "true" else "false")
            , ( "route"  , jsonScalar . r . tm . PlayR $ plNo item                )
            ]

-- Routes {{{
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

-- Helpers {{{

-- | Given the current song, length of current playlist, and how many 
--   lines of context to show, return the correct upper and lower bounds
--   to send to a MPD.playListInfo command to get the context playlist
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

getTag :: MPD.Metadata -> MPD.Song -> String
getTag tag = head . fromMaybe ["N/A"] . MPD.sgGet tag

-- | Conditionally apply the monadic f to the maybe value when it is 
--   Just, wrap the /inner/ value in Just, or return Nothing
(=<?) :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
f =<? (Just a) = return . Just =<< f a
f =<?  Nothing = return Nothing

-- }}}
