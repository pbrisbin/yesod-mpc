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
            let artist = shorten 20 $ getTag MPD.Artist song
            let album  = shorten 20 $ getTag MPD.Album  song
            let sPos   = fromMaybe (-1) . fmap fst $ MPD.sgIndex song
            let sId    = fromMaybe (-1) . fmap snd $ MPD.sgIndex song

            coverurl <- albumArtHelper (artist,album)
            playlist <- do
                len    <- return . either (const 0) length =<< withMPD (MPD.playlistInfo Nothing)
                result <- withMPD $ MPD.playlistInfo (Just $ fixBounds sPos len 10)
                case result of
                    Left _      -> return []
                    Right songs -> return $ map (itemFromSong sId) songs

            return $ Just NowPlaying
                { npTitle    = getTag MPD.Title song
                , npArtist   = artist
                , npAlbum    = album
                , npYear     = getTag MPD.Date song
                , npState    = case MPD.stState state of
                    MPD.Playing -> "playing"
                    MPD.Paused  -> "paused"
                    MPD.Stopped -> "stopped"
                , npPos      = sPos
                , npId       = sId
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
                { plArtist  = shorten 20 $ getTag MPD.Artist song
                , plAlbum   = shorten 20 $ getTag MPD.Album  song
                , plTitle   = shorten 30 $ getTag MPD.Title  song
                , plNo      = num
                , plPlaying = cid == num
                , plRoute   = PlayR num
                }

        shorten :: Int -> String -> String
        shorten n s = if length s > n then take n s ++ "..." else s

-- | Main page
getStatusR :: YesodMPC m => GHandler MPC m RepHtmlJson
getStatusR = do
    authHelper
    tm  <- getRouteToMaster
    r   <- getUrlRender
    mnp <- nowPlaying

    -- reply with main html view or json updates
    defaultLayoutJson (htmlRep tm mnp) (jsonRep tm r mnp)
        
    where
        htmlRep toMaster (Just np) = do
            jQuery <- lift jQueryUrl
            delay  <- lift $ fmap (*1000) refreshSpeed
           
            setTitle $ toHtml ("MPD - " ++ npTitle np)

            let playing = npState np == "playing"
            
            -- javascript {{{
            addJulius [$julius|
                /* click events */
                function buttonEvent(e) {
                    var prevR  = "@{toMaster PrevR}";
                    var pauseR = "@{toMaster PauseR}";
                    var nextR  = "@{toMaster NextR}";

                    switch(e.id) {
                        case "mpc_prev":
                            $.getJSON(prevR, {}, ajaxUpdate);
                            break;

                        case "mpc_play":
                        case "mpc_pause":
                            $.getJSON(pauseR, {}, ajaxUpdate);
                            break;

                        case "mpc_next":
                            $.getJSON(nextR, {}, ajaxUpdate);
                            break;
                    }

                    // disable normal behavior
                    return false;
                }

                /* repopulate the playlist table on track changes */
                function updatePlaylist(playlist) {
                    var html = "";

                    html += "<table>";
                    html += "<tr><th>No</th><th>Artist</th><th>Album</th><th>Title</th></tr>";

                    for (var i in playlist) {
                        var item = playlist[i];

                        // Yesod.Json only sends strings
                        html += item.playing == "true" ? '<tr class="mpc_current">' : '<tr>';

                        html += '<td><a href="' + item.route + '">' + item.no + "</a></td>"
                             +  "<td>" + item.artist + "</td>"
                             +  "<td>" + item.album  + "</td>"
                             +  "<td>" + item.title  + "</td>";

                        html += "</tr>";
                    }
                    
                    html += "</table>";

                    $(".mpc_playlist").html(html);
                }

                /* callback for updated now playing info */
                function ajaxUpdate(o) {
                    if (o.status != "OK") {
                        // mpd threw some error, bail
                        return;
                    }

                    if (o.pos != $("#mpc_pos").text() || o.id  != $("#mpc_id").text()) {
                        // track's changed, more needs to update
                        var t = $("#mpc_title");

                        if (t && o.title != t.text()) {
                            t.text(o.title);
                            document.title = "MPD - " + o.title;
                        }

                        var a = $("#mpc_artist");
                        var b = $("#mpc_album");
                        var y = $("#mpc_year");

                        if (a && o.artist != a.text()) a.text(o.artist);
                        if (b && o.album  != b.text()) b.text(o.album);
                        if (y && o.year   != y.text()) y.text(o.year);

                        // update cover image
                        var c = $("#mpc_cover");

                        if (c && o.coverurl != c.attr("src")) {
                            if (o.coverurl)
                                c.attr("src", o.coverurl).css( { display: "block" } );
                            else
                                c.css( { display: "none" } );
                        }

                        updatePlaylist(o.playlist);
                    }

                    // these update every time
                    $("#mpc_progress_inner").css( { width: o.progress + "%" } );

                    var cu = $("#mpc_cur");
                    var to = $("#mpc_tot");

                    if (cu && o.cur != cu.text()) cu.text(o.cur);
                    if (to && o.tot != to.text()) to.text(o.tot);

                    var s = $("#mpc_state");

                    if (s && o.state != s.text()) {
                        s.text(o.state);

                        // update play/pause button
                        switch (o.state) {
                            case "playing":
                                $("#mpc_play").css(  { display: "none"         });
                                $("#mpc_pause").css( { display: "inline-block" });
                                break;

                            default:
                                $("#mpc_play").css(  { display: "inline-block" });
                                $("#mpc_pause").css( { display: "none"         });
                                break;
                        }
                    }
                }

                /* called on document to kick it off */
                function getNowPlaying() {
                    var delay = #{show delay}; // server-set

                    if (delay != 0) {
                        $.getJSON(window.location.href, {}, ajaxUpdate);
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
                input.mpc_pp
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

                <div .mpc_controls>
                    <table>
                        <tr>
                            <td>
                                <input #mpc_prev type="submit" value="&#9664;&#9664;" onclick="buttonEvent(this);">
                            <td>
                                $if playing
                                    <input .mpc_pp #mpc_pause style="display: inline-block;" type="submit" value="&#9655;" onclick="buttonEvent(this);">
                                    <input .mpc_pp #mpc_play  style="display: none;" type="submit" value="&#9654;" onclick="buttonEvent(this);">
                                $else
                                    <input .mpc_pp #mpc_pause style="display: inline-block;" type="submit" value="&#9655;" onclick="buttonEvent(this);">
                                    <input .mpc_pp #mpc_play  style="display: none;" type="submit" value="&#9654;" onclick="buttonEvent(this);">
                            <td>
                                <input #mpc_next type="submit" value="&#9654;&#9654;" onclick="buttonEvent(this);">

                <div #mpc_progress_outer>
                    <div #mpc_progress_inner>&nbsp;


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
