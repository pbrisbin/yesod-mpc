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
    , PlaylistItem(..)
    ) where

import Yesod
import Text.Blaze (toHtml)

import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax hiding (lift)

import qualified Network.MPD as MPD

-- | Represents now playing state, see 'nowPlaying' for how this is 
--   constructed when needed.
data NowPlaying url = NowPlaying
    { npState    :: String
    , npTitle    :: String
    , npArtist   :: String
    , npAlbum    :: String
    , npYear     :: String
    , npPos      :: Int          -- ^ position in playlist
    , npId       :: Int          -- ^ playlist id
    , npCur      :: String       -- ^ time elapsed, MM:SS
    , npTot      :: String       -- ^ song length, MM:SS
    , npProg     :: Double       -- ^ percentage elapsed for convenience
    , npCover    :: Maybe String -- ^ url to cover image if setup
    , npPlaylist :: [PlaylistItem url]
    }

-- | An item in the playlist, a few extra values from NowPlaying for 
--   convenience
data PlaylistItem url = PlaylistItem
    { plNo      :: Int
    , plArtist  :: String
    , plAlbum   :: String
    , plTitle   :: String
    , plPlaying :: Bool -- ^ item is currently playing
    , plRoute   :: url  -- ^ route to play this track
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

-- | Return now playing info or nothing. Includes playlist info as 10 
--   lines of context with currently playing in the middle
nowPlaying :: YesodMPC m => GHandler s m (Maybe (NowPlaying MPCRoute))
nowPlaying = do
    songResp  <- withMPD MPD.currentSong
    stateResp <- withMPD MPD.status
    case (songResp,stateResp) of
        (Right (Just song), Right state) -> do

            let artist = getTag MPD.Artist song
            let album  = getTag MPD.Album  song
            let pos    = fromMaybe (-1) . fmap fst $ MPD.sgIndex song
            let id     = fromMaybe (-1) . fmap snd $ MPD.sgIndex song

            playlist <- do
                len <- return . either (const 0) length =<< withMPD (MPD.playlistInfo Nothing)
                let bounds = Just $ fixBounds (pos) len 10 -- increase context here if desired

                result <- withMPD $ MPD.playlistInfo bounds
                case result of
                    Left _      -> return []
                    Right songs -> return $ map (itemFromSong $ id) songs

            coverurl <- albumArtHelper (artist,album)

            return $ Just NowPlaying
                { npTitle    = getTag MPD.Title song
                , npArtist   = artist
                , npAlbum    = album
                , npYear     = getTag MPD.Date song
                , npState    = case MPD.stState state of
                    MPD.Playing -> "playing"
                    MPD.Paused  -> "paused"
                    MPD.Stopped -> "stopped"
                , npPos      = pos
                , npId       = id
                , npCur      = format . round . fst $ MPD.stTime state
                , npTot      = format .         snd $ MPD.stTime state
                , npProg     = progress $ MPD.stTime state
                , npCover    = coverurl
                , npPlaylist = playlist
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

        itemFromSong :: Int -> MPD.Song -> PlaylistItem MPCRoute
        itemFromSong cid song = let num = fromMaybe 0 . fmap snd $ MPD.sgIndex song
            in PlaylistItem
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

    tm <- getRouteToMaster
    r  <- getUrlRender

    -- reply with main html view or json updates
    defaultLayoutJson (htmlRep tm mnp) (jsonRep tm r mnp)
        
    where
        htmlRep toMaster (Just np) = do
            jQuery <- lift jQueryUrl
            delay  <- lift $ fmap (*1000) refreshSpeed
           
            setTitle $ toHtml ("MPD - " ++ npTitle np)
            
            -- javascript {{{
            addJulius [$julius|
                function buttonEvent(e) {
                    var prevR  = "@{toMaster PrevR}";
                    var pauseR = "@{toMaster PauseR}";
                    var nextR  = "@{toMaster NextR}";

                    switch(e.id) {
                        case "mpc_prev":
                            $.getJSON(prevR, {}, function(o) { return true; });
                            break;

                        case "mpc_pp":
                            $.getJSON(pauseR, {}, function(o) { return true; });
                            break;

                        case "mpc_next":
                            $.getJSON(nextR, {}, function(o) { return true; });
                            break;
                    }

                    // disable normal behavior
                    return false;
                }

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
                                    if (o.tot   != $("#mpc_tot").text())   $("#mpc_tot").text(o.tot);
                                    if (o.cur   != $("#mpc_cur").text())   $("#mpc_cur").text(o.cur);
                                    if (o.state != $("#mpc_state").text()) $("#mpc_state").text(o.state);

                                    // update progress
                                    $("#mpc_progress_inner").css( { width: o.progress + "%" } );

                                    // update PP button
                                    /* todo: html entities aren't working right
                                    var pp = $("#mpc_pp");

                                    if (pp && !pp.hasClass(o.state)) {
                                        pp.removeClass();
                                        pp.addClass(o.state);

                                        switch (o.state) {
                                            case "playing":
                                                pp.attr("value", "||");
                                                break;

                                            default:
                                                pp.attr("value","&#9654;");
                                                break;
                                        }
                                    }
                                    */
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
                                    if (o.coverurl != $("#mpc_cover").attr("src")) {
                                        if (o.coverurl) {
                                            $("#mpc_cover").css( { display: "block" } ).attr("src", o.coverurl);
                                        }
                                        else {
                                            $("#mpc_cover").css( { display: "none" } );
                                        }
                                    }

                                    // update progress
                                    $("#mpc_progress_inner").css( { width: o.progress + "%" } );

                                    // update PP button
                                    /* todo: htmlentities aren't working...
                                    var pp = $("#mpc_pp");

                                    if (pp && !pp.hasClass(o.state)) {
                                        pp.removeClass();
                                        pp.addClass(o.state);

                                        switch (o.state) {
                                            case "playing":
                                                pp.val("||");
                                                break;

                                            default:
                                                pp.val("&#9654");
                                                break;
                                        }
                                    }
                                    */

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
                .mpc_playlist a:link, .mpc_playlist a:visited, .mpc_playlist a:hover
                    outline:         none
                    text-decoration: none
                tr.mpc_current
                    font-weight: bold
                .mpc_controls table
                    margin:  0px auto;
                .mpc_controls
                    font-size: 115%;
                input#mpc_pp
                    font-size: 130%;
                    padding: 0px 1ex;
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
                        <img #mpc_cover style="display: block;" src="#{cover}">
                    $nothing
                        <img #mpc_cover style="display: none;" src="">

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

                        $forall item <- npPlaylist np
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
                                <input #mpc_prev type="submit" value="&#9664;&#9664;" onclick="buttonEvent(this);">
                            <td>
                                <input #mpc_pp .paused type="submit" value="&#9654;" onclick="buttonEvent(this);">
                            <td>
                                <input #mpc_next type="submit" value="&#9654;&#9654;" onclick="buttonEvent(this);">

                \<!-- end player controls }}} -->

                \<!-- progress bar {{{ -->
                <div #mpc_progress_outer>
                    <div #mpc_progress_inner>&nbsp;

                \<!-- end progress bar }}} -->

                <script src="#{jQuery}">
                <script>$(function() { getNowPlaying(); });

                |]
                -- }}}

        htmlRep _ _  = do
            setTitle $ toHtml "MPD"
            [$hamlet|
                <h1>MPD
                <div .mpc_error>
                    <p>
                        <em>Unable to determine now playing info.
                |]

        jsonRep tm r (Just np) = jsonMap
            [ ("status"  , jsonScalar $ "OK"                             )
            , ("state"   , jsonScalar $ npState np                       )
            , ("title"   , jsonScalar $ npTitle np                       )
            , ("artist"  , jsonScalar $ npArtist np                      )
            , ("album"   , jsonScalar $ npAlbum np                       )
            , ("year"    , jsonScalar $ npYear np                        )
            , ("pos"     , jsonScalar . show $ npPos np                  )
            , ("id"      , jsonScalar . show $ npId np                   )
            , ("cur"     , jsonScalar $ npCur np                         )
            , ("tot"     , jsonScalar $ npTot np                         )
            , ("progress", jsonScalar . show $ npProg np                 )
            , ("coverurl", jsonScalar . fromMaybe "" $ npCover np        )
            , ("playlist", jsonList $ map (jsonItem tm r) (npPlaylist np))
            ]
                
        jsonRep _ _ _ = jsonMap
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
getPrevR :: YesodMPC m => GHandler MPC m RepHtmlJson
getPrevR = authHelper >> actionRoute MPD.previous

-- | Smart play/pause button
getPauseR :: YesodMPC m => GHandler MPC m RepHtmlJson
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
getNextR :: YesodMPC m => GHandler MPC m RepHtmlJson
getNextR = authHelper >> actionRoute MPD.next

-- | Play a specific song in playlist
getPlayR :: YesodMPC m => Int -> GHandler MPC m RepHtmlJson
getPlayR pid = authHelper >> actionRoute (MPD.playId pid)

getDelR :: YesodMPC m => Int -> GHandler MPC m RepHtmlJson
getDelR pid = authHelper >> actionRoute (MPD.deleteId pid)

-- | Execute any mpd action then redirect back to the main status page
actionRoute :: YesodMPC m => MPD.MPD a -> GHandler MPC m RepHtmlJson
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

-- }}}
