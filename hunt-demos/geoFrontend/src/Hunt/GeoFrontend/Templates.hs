{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hunt.GeoFrontend.Templates where

import qualified Text.Hamlet as Hamlet (HtmlUrl, hamlet)
import qualified Text.Blaze.Html.Renderer.String as Blaze (renderHtml)
--import qualified Text.Blaze.Html as Blaze (Html)
import Data.Text.Lazy (Text)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T

import qualified Hunt.GeoFrontend.Common as Api
import qualified Hunt.Server.Client as Api

data Routes = Home | GeoJs | GeoCSS | Autocomplete | About

render :: Routes -> [(TS.Text, TS.Text)] -> TS.Text
render Home _ = "/"
render GeoJs _ = "/geoFrontend.js"
render GeoCSS _ = "/geoFrontend.css"
render Autocomplete _ = "/autocomplete"
render About _ = "/about"

renderTitle :: Text
renderTitle = "Hunt Geo Demo"

--header :: Blaze.Html
header :: Hamlet.HtmlUrl Routes
header = [Hamlet.hamlet|
  <head>

    <title>#{renderTitle}
    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js">
    <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js">
    <link href="//code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css" rel="stylesheet">

    <script src="//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js">
    <link href="//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css" rel="stylesheet">

    <link rel="stylesheet" href="//cdn.leafletjs.com/leaflet-0.7.2/leaflet.css" />
    <script src="//cdn.leafletjs.com/leaflet-0.7.2/leaflet.js">

    <link href=@{GeoCSS} rel="stylesheet">
    <script src=@{GeoJs}>

    <meta name="viewport" content="width=device-width, initial-scale=1.0">

    <script type="javascript">
        var map = L.map('map').setView([51.505, -0.09], 13);
|]

navigation :: Text -> Hamlet.HtmlUrl Routes
navigation query = [Hamlet.hamlet|
<div .navbar .navbar-default .navbar-static-top role="navigation">

    <div .navbar-header .navbar-left>
        <a href=@{Home}>
            <img .logo src="//holumbus.fh-wedel.de/hayoo/hayoo.png" alt="Hunt Searchengine logo" >
        <button type="button" .navbar-toggle data-toggle="collapse" data-target="#geoFrontend-navbar-collapse">
            <span .sr-only>Toggle navigation
            <span .icon-bar>
            <span .icon-bar>
            <span .icon-bar>

    <div .navbar-collapse .collapse #geoFrontend-navbar-collapse>
        <ul .nav .navbar-nav .navbar-left>
            <li .active>
                <form .navbar-form .navbar-left action="javascript:void(0);" method="get" id="search" role="search">
                    <div .form-group>
                        <input .form-control placeholder="Search" name="query" #geoFrontend type="text" autocomplete="off" accesskey="1" value="#{query}">
                    <input .btn .btn-default #submit type="submit" value="Search">

        <ul .nav .navbar-nav .navbar-right>
            <li>
                <a href=@{About}>About
|]


footer :: Hamlet.HtmlUrl Routes
footer = [Hamlet.hamlet|
<footer>

    <a href=@{Home}> Hunt Searchengine
|]

body :: Text -> T.Text
body query = T.pack $ Blaze.renderHtml $ [Hamlet.hamlet|
$doctype 5
<html lang="en">
    ^{header}
    <body>
        ^{navigation query}

        <div .container #map style="height:380">

        ^{footer}
|] render
