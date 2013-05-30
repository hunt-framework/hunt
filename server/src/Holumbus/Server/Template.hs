module Holumbus.Server.Template (
  index 
) where
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

import           Text.Hamlet
import           Text.Blaze.Html.Renderer.Text (renderHtml)

-- | main page
index = renderHtml . defaultLayout $ [xshamlet|
<h1>Holumbus Server
<hr>
<form>
  <div .input-append>
    <input .span6 type=text name=search id=search > 
    <button .btn .btn-primary type=button>Search
<div .well>
  results...
<hr>
<form>
  <textarea .span6 name=document id=document style=height:100px>
    {"desc":{"title":"empty document"},"uri":"id::1","words":{"defaultContext":{"word":[]}}}
  <button .btn .btn-primary>
    Add Document
|]

-- | default layout
--defaultLayout :: forall a.
--                 Text.Blaze.ToMarkup a =>
--                 a -> Text.Blaze.Internal.MarkupM ()
defaultLayout content = [xshamlet|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Holumbus Server
    <script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js" type="text/javascript">
    <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet">
  <body>
    <div .container>
      #{content}
|]


