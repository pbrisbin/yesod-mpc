{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
    -- * Subsite
    , MPC
    , getMPC
    , MPCRoute(..)
    -- * MPD interface
    , NowPlaying(..)
    , nowPlaying
    , PlaylistItem(..)
    , contextPlaylist
    ) where

import Yesod
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax hiding (lift)

import Network.MPD (State(..), Title, Artist, Album)

import qualified Network.MPD as MPD
import qualified Data.Text   as T

data MPC = MPC

getMPC :: a -> MPC
getMPC = const MPC

class Yesod m => YesodMPC m where
    -- | Supply the @withMPD@ function for connecting to your MPD 
    --   server.
    withMPD :: Yesod m => MPD.MPD a -> GHandler s m (MPD.Response a)
    withMPD = liftIO . MPD.withMPD

    -- | default is return (), don't authenticate
    authHelper :: GHandler s m ()
    authHelper = return ()

    -- | default is Nothing
    albumArtHelper :: (Artist, Album) -> GHandler s m (Maybe String)
    albumArtHelper = return . const Nothing

    -- | seconds, default is 1, return 0 to disable
    refreshSpeed :: GHandler s m Int
    refreshSpeed = return 1

    -- | In case you serve your own jQuery, default is hosted by google
    jQueryUrl :: GHandler s m String
    jQueryUrl = return "http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"

mkYesodSub "MPC" 
    [ ClassP ''YesodMPC [ VarT $ mkName "master" ] ] 
    [parseRoutes|
        /            StatusR  GET
        /prev        PrevR    GET
        /next        NextR    GET
        /play/#Int   PlayR    GET
        /delete/#Int DelR     GET
        /pause       PauseR   GET
        /repeat      RepeatR  GET
        /random      RandomR  GET
        |]

-- | Represents now playing state, see 'nowPlaying' for how this is 
--   constructed when needed.
data NowPlaying = NowPlaying
    { npState    :: State
    , npTitle    :: Title
    , npArtist   :: Artist
    , npAlbum    :: Album
    , npYear     :: String
    , npPos      :: Int            -- ^ position in playlist
    , npId       :: Int            -- ^ id in playlist
    , npCur      :: String         -- ^ time elapsed, MM:SS
    , npTot      :: String         -- ^ song length, MM:SS
    , npProg     :: Double         -- ^ percentage elapsed (for convenience)
    , npCover    :: Maybe String   -- ^ url to cover image if setup
    , npPlaylist :: [PlaylistItem] -- ^ playlist context
    }

-- | An item in the playlist, a few extra values from NowPlaying for 
--   convenience
data PlaylistItem = PlaylistItem
    { plNo      :: Int
    , plArtist  :: Artist
    , plAlbum   :: Album
    , plTitle   :: Title
    , plPlaying :: Bool     -- ^ item is currently playing
    , plRoute   :: MPCRoute -- ^ route to play this track
    }

-- | Return now playing info or nothing
nowPlaying :: YesodMPC m => GHandler s m (Maybe NowPlaying)
nowPlaying = do
    songResp  <- withMPD MPD.currentSong
    stateResp <- withMPD MPD.status

    case (songResp, stateResp) of
        (Right (Just song), Right state) -> do
            let artist = shorten 20 $ getTag MPD.Artist song
            let album  = shorten 20 $ getTag MPD.Album  song
            let sPos   = fromMaybe (-1) $ MPD.stSongPos state
            let sId    = fromMaybe (-1) $ MPD.stSongID  state

            playlist <- contextPlaylist sPos sId 10 -- limit
            coverurl <- albumArtHelper (artist, album)

            return $ Just NowPlaying
                { npTitle    = getTag MPD.Title song
                , npArtist   = artist
                , npAlbum    = album
                , npYear     = getTag MPD.Date song
                , npState    = MPD.stState state
                , npPos      = sPos
                , npId       = sId
                , npCur      = format . round . fst $ MPD.stTime state
                , npTot      = format .         snd $ MPD.stTime state
                , npProg     = progress $ MPD.stTime state
                , npCover    = coverurl
                , npPlaylist = playlist
                } 

        -- todo: make this an Either and return the error(s)?
        _ -> return Nothing

    where
        -- convert seconds to MM:SS
        format = (\(q,r) -> pad q ++ ":" ++ pad r) . flip divMod 60
        pad    = (\s -> if length s == 1 then '0':s else s) . show

        -- convert stTime into percentage
        progress :: (Double, MPD.Seconds) -> Double
        progress (d,i) = (*100) (d / realToFrac i)

-- | Make a \"context\" playlist where the playing track is in the 
--   middle with tracks above and below, limited in total number
contextPlaylist :: YesodMPC m
                => Int -- ^ Position of currently playing track
                -> Int -- ^ Id of currently playing track
                -> Int -- ^ Lines of context to show
                -> GHandler s m [PlaylistItem]
contextPlaylist pos cid lim = return . helper =<< withMPD (MPD.playlistInfo Nothing)

    where
        helper (Left  _  ) = []
        helper (Right res) = map (itemFromSong cid) . take lim $ drop (getDrop pos (length res) lim) res

        getDrop 0   _   _     = 0
        getDrop _   0   _     = 0
        getDrop _   _   0     = 0
        getDrop pos len limit = let 
            half = limit `div` 2 
            bot  = pos - (half - 1) in go bot (bot + limit)
            where
                go b t
                    | b <= 0    = 0
                    | t > len   = getDrop (pos - 1) len limit
                    | otherwise = b - 1

        itemFromSong :: Int -> MPD.Song -> PlaylistItem
        itemFromSong cId song = let num = fromMaybe 0 $ MPD.sgIndex song
            in PlaylistItem
                { plArtist  = shorten 20 $ getTag MPD.Artist song
                , plAlbum   = shorten 20 $ getTag MPD.Album  song
                , plTitle   = shorten 30 $ getTag MPD.Title  song
                , plNo      = num
                , plPlaying = num == cId
                , plRoute   = PlayR num
                }

-- | Main page {{{
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

            let playing = npState np == Playing
            
            -- javascript {{{
            addJulius [julius|
                // top-level, server-set variable
                var delay = #{show delay};

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
                            case "Playing":
                                $("#mpc_play").css(  { display: "none"         });
                                $("#mpc_pause").css( { display: "inline-block" });
                                break;

                            default:
                                $("#mpc_play").css(  { display: "inline-block" });
                                $("#mpc_pause").css( { display: "none"         });
                                break;
                        }
                    }

                    setTimeout("getNowPlaying();", delay);
                }

                /* called on document to kick it off */
                function getNowPlaying() {
                    if (delay != 0) {
                        $.getJSON(window.location.href, {}, ajaxUpdate);
                    }
                }
            |]
            -- }}}
            
            -- css {{{
            addCassius [cassius|
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
            [hamlet|
                <h1>MPD

                <div .mpc_nowplaying>
                    $maybe cover <- npCover np
                        <img #mpc_cover style="display: block;" src="#{cover}">
                    $nothing
                        <img #mpc_cover style="display: none;" src="">

                    <p>
                        <span #mpc_pos style="display: none;">#{show $ npPos np}
                        <span #mpc_id style="display: none;">#{show $ npId np}

                        <span #mpc_state>#{show $ npState np}
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
            setTitle "MPD"
            [hamlet|
                <h1>MPD
                <div .mpc_error>
                    <p>
                        <em>Unable to determine now playing info.
                |]

        jsonRep tm r (Just np) = jsonMap
            [ ("status"  , jsonScalar $ "OK"                             )
            , ("title"   , jsonScalar $ npTitle np                       )
            , ("artist"  , jsonScalar $ npArtist np                      )
            , ("album"   , jsonScalar $ npAlbum np                       )
            , ("year"    , jsonScalar $ npYear np                        )
            , ("cur"     , jsonScalar $ npCur np                         )
            , ("tot"     , jsonScalar $ npTot np                         )
            , ("state"   , jsonScalar . show $ npState np                )
            , ("pos"     , jsonScalar . show $ npPos np                  )
            , ("id"      , jsonScalar . show $ npId np                   )
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
            , ( "route"  , jsonScalar . T.unpack . r . tm . PlayR $ plNo item     )
            ]

-- }}}

getPrevR :: YesodMPC m => GHandler MPC m RepHtmlJson
getPrevR = actionRoute MPD.previous

getNextR :: YesodMPC m => GHandler MPC m RepHtmlJson
getNextR = actionRoute MPD.next

getPlayR :: YesodMPC m => Int -> GHandler MPC m RepHtmlJson
getPlayR = actionRoute . MPD.playId

getDelR :: YesodMPC m => Int -> GHandler MPC m RepHtmlJson
getDelR = actionRoute . MPD.deleteId

getPauseR :: YesodMPC m => GHandler MPC m RepHtmlJson
getPauseR = toggleRoute MPD.stState flipPP
    where
        flipPP Playing = MPD.pause True
        flipPP _       = MPD.play  Nothing

getRepeatR :: YesodMPC m => GHandler MPC m RepHtmlJson
getRepeatR = toggleRoute MPD.stRepeat (MPD.repeat . not)

getRandomR :: YesodMPC m => GHandler MPC m RepHtmlJson
getRandomR = toggleRoute MPD.stRandom (MPD.random . not)

-- | Execute any mpd action then redirect back to status page
actionRoute :: YesodMPC m => MPD.MPD () -> GHandler MPC m RepHtmlJson
actionRoute f = do
    authHelper
    _  <- withMPD f
    tm <- getRouteToMaster
    redirect RedirectTemporary $ tm StatusR

-- | Toggle any mpd setting then redirect back to status page
toggleRoute :: YesodMPC m
            => (MPD.Status -> a)  -- ^ how to get from state to current setting
            -> (a -> MPD.MPD ())  -- ^ how to set new setting based on current state
            -> GHandler MPC m RepHtmlJson
toggleRoute from to = go . fmap from =<< withMPD MPD.status
    where
        go (Right v) = actionRoute $ to v
        go _         = actionRoute $ return ()

-- | Return the first requested metadata tag or \"N/A\"
getTag :: MPD.Metadata -> MPD.Song -> String
getTag tag = head . fromMaybe ["N/A"] . MPD.sgGetTag tag

-- | Truncate at desired length and add elipsis if truncated
shorten :: Int -> String -> String
shorten n s = if length s > n then take n s ++ "..." else s
