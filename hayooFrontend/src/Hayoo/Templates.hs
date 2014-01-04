{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hayoo.Templates where

import qualified Text.Hamlet as Hamlet (HtmlUrl, hamlet)
import qualified Text.Julius as Julius
import qualified Text.Blaze.Html.Renderer.String as Blaze (renderHtml)
--import qualified Text.Blaze.Html as Blaze (Html)
import Data.Text (Text)
import qualified Data.Text.Lazy as TextL

import qualified Hayoo.ApiClient as Api

data Routes = Home | HayooJs | HayooCSS | Autocomplete

render :: Routes -> [(Text, Text)] -> Text
render Home _ = "/"
render HayooJs _ = "/hayoo.js"
render HayooCSS _ = "/hayoo.css"
render Autocomplete _ = "/autocomplete"

renderTitle :: String -> String
renderTitle query
    | null query = "Hayoo! Haskell API Search"
    | otherwise = query ++ " - Hayoo!"

--header :: Blaze.Html
header :: String -> Hamlet.HtmlUrl Routes
header query = [Hamlet.hamlet|
  <head>
    <title>#{renderTitle query}
    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js">
    <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js">
    <link href="//code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css" rel="stylesheet">

    <script src="//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js">
    <link href="//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css" rel="stylesheet">
    
    <link href=@{HayooCSS} rel="stylesheet">
    <script src=@{HayooJs}>
    
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
|]

navigation :: String -> Hamlet.HtmlUrl Routes
navigation query = [Hamlet.hamlet|
<div .navbar .navbar-default .navbar-static-top role="navigation">

    <div .navbar-header .navbar-left>
        <a href=@{Home}>
            <img .logo src="//holumbus.fh-wedel.de/hayoo/hayoo.png" alt="Hayoo! logo" >
        <button type="button" .navbar-toggle data-toggle="collapse" data-target="#hayoo-navbar-collapse">
            <span .sr-only>Toggle navigation
            <span .icon-bar>
            <span .icon-bar>
            <span .icon-bar>
       
    <div .navbar-collapse .collapse #hayoo-navbar-collapse>
        <ul .nav .navbar-nav .navbar-left>
            <li .active>
                <form .navbar-form .navbar-left action="." method="get" id="search" role="search">
                    <div .form-group>
                        <input .form-control placeholder="Search" name="query" #hayoo type="text" autocomplete="off" accesskey="1" value="#{query}">
                    <input .btn .btn-default #submit type="submit" value="Search">

        <ul .nav .navbar-nav .navbar-right>
            <li .active>
                <a href="/help">Help
            <li >
                <a href="/examples">Examples
            <li>
                <a href="/About">About
|]


footer :: Hamlet.HtmlUrl Routes
footer = [Hamlet.hamlet|
<footer>
    <a href=@{Home}> Hayoo Frontend #
    by Sebastian Philipp
|]

body :: String -> Hamlet.HtmlUrl Routes -> TextL.Text 
body query content = TextL.pack $ Blaze.renderHtml $ [Hamlet.hamlet|
$doctype 5
<html lang="en">
    ^{header query}
    <body>
        ^{navigation query}
        
        <div class="container">
            ^{content}
        
        ^{footer}
|] render

renderResult :: Api.SearchResult -> Hamlet.HtmlUrl Routes
renderResult (Api.FunctionResult u p m n s d) = [Hamlet.hamlet|
<div .panel .panel-default>
    <div .panel-heading>
        <a href=#{u}>
            #{n}
        :: #{s}
    <div .panel-body>
        <p>
            #{p} - #{m}
    <div .panel-body>
        <p>
            #{d}
|]

renderLimitedRestults :: Api.LimitedResult Api.SearchResult -> Hamlet.HtmlUrl Routes
renderLimitedRestults limitedRes = [Hamlet.hamlet|
<ul .list-group>
    $forall result <- Api.lrResult limitedRes
        <li .list-group-item>
            ^{renderResult result} 
|]

mainPage :: Hamlet.HtmlUrl Routes
mainPage = [Hamlet.hamlet|
<div .jumbotron>
  <h1>
      Welcome!
|]

hayooJs :: TextL.Text -- Julius.JavascriptUrl Routes
hayooJs = Julius.renderJavascript $ 
    $(Julius.juliusFileReload "/home/privat/holumbus/hayooFrontend/data/hayoo.lucius") render


