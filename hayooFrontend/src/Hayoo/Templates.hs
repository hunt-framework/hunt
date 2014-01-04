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

import Hayoo.ApiClient

data Routes = Home | HayooJs | HayooCSS | Autocomplete

render :: Routes -> [(Text, Text)] -> Text
render Home _ = "/"
render HayooJs _ = "/hayoo.js"
render HayooCSS _ = "/hayoo.css"
render Autocomplete _ = "/autocomplete"

--header :: Blaze.Html
header :: Hamlet.HtmlUrl Routes
header = [Hamlet.hamlet|
  <head>
    <title>Hayoo Cabal API Search
    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js">
    <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js">
    <link href="//code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css" rel="stylesheet">

    <script src="//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js">
    <link href="//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css" rel="stylesheet">
    
    <link href=@{HayooCSS} rel="stylesheet">
    <script src=@{HayooJs}>
    
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
|]

footer :: Hamlet.HtmlUrl Routes
footer = [Hamlet.hamlet|
<footer>
    <a href=@{Home}> Hayoo Frontend#
    by Sebastian Philipp
|]




body :: String -> Hamlet.HtmlUrl Routes -> TextL.Text 
body query content = TextL.pack $ Blaze.renderHtml $ [Hamlet.hamlet|
$doctype 5
<html lang="en">
    ^{header}
    <body>
        <div .navbar .navbar-default .navbar-static-top role="navigation">
          <div .container>
            <div .navbar-header>
              <a navbar-brand href=@{Home}>
                <img .logo src="//holumbus.fh-wedel.de/hayoo/hayoo.png" alt="Hayoo! logo" >
            <div .navbar-collapse .collapse>
              <ul .nav .navbar-nav>
                <li .active>
                    <form action="." method="get" id="search">
                        <div .ui-widget>
                            <input name="query" #hayoo type="text" autocomplete="off" accesskey="1" value="#{query}">
                            <input #submit type="submit" value="Search">
                <li>
                    <a href="#about">About
                <li>
                    <a href="#contact">Contact
                
              <ul .nav .navbar-nav .n.avbar-right>
                <li>
                    <a href="../navbar/">Default
                <li .active>
                    <a href="./">Static top
                <li>
                    <a href="../navbar-fixed-top/">Fixed top
        
        ^{content}
        
        ^{footer}
|] render

renderResult :: SearchResult -> Hamlet.HtmlUrl Routes
renderResult (FunctionResult u p m n s d) = [Hamlet.hamlet|
<div .panel .panel-default>
    <div .panel-heading>
        <a href=#{u}>
            #{n}
    <div .panel-body>
        <p>
            #{p} - #{m}
    <div .panel-body>
        <p>
            #{d}
|]

renderLimitedRestults :: LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
renderLimitedRestults limitedRes = [Hamlet.hamlet|
<ul .list-group>
    $forall result <- lrResult limitedRes
        <li>^{renderResult result} 
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


