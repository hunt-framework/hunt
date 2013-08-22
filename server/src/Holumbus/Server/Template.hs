module Holumbus.Server.Template
( index
, addDocs
) where
--import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

import           Text.Hamlet
import           Text.Julius
import           Text.Blaze.Html.Renderer.Text    (renderHtml)
import           Text.Blaze                       (Markup, ToMarkup)

-- | main page
index :: LT.Text
index =
  -- generate html with hamlet
  (renderHtml . defaultLayout $ [xshamlet|
<form>
  <div .input-append >
    <input .input-xxlarge type=text #txt-search>
    <button .btn .btn-primary type=button #btn-search>Search
<hr>
<div  #result>
  &nbsp;
<hr>
|]) `LT.append`
  -- generate javascript
  renderJavascriptUrl (\_ _ -> "") [julius|
<script>
  $(document).ready(function() {

    $(document).keypress(function(event) {
      if ( event.which == 13 ) {
         event.preventDefault();
         search(event);
      }
    });

    $("#txt-search").typeahead({
      source: function(query, callback) {
        $.get("/completion/" + query, function(data) {
          var result = [];
          if (data.code === 0)
          {
            $(data.msg).each(function(i,e) {
              result.push(e[0]);
            });
          }
          callback(result);
        })
      }
    });

    /* search button handler */
    $("#btn-search").click(function(e){
      search(e);
    });

    var search = function(ev){
      ev.preventDefault();
      var query = $("#txt-search").val();

      if (query === "") {
         $("#result").html("<p>empty search query ...</p>");
         return false;
      }

      $("#result").html("<p>searching ...</p>");
      $.get("/search/" + query, function(data) {
        if (data.code === 0)
        {
          var docs = data.msg;
          if (docs.length === 0)
          {
            $("#result").html("<p>no results found :-(</p>");
            return false;
          }

          var res = '<table class="table table-bordered">';
          $(docs).each(function(i,e) {
             res += "<tr><td>" + e.uri + "</td><td>";
             var desc = e.desc
             for (var key in desc){
               res += "<p>" + key + ":" + desc[key] + "</p>";
             }
             res += "</td></tr>";
          });
          res += "</table>";
          $("#result").html(res);
        }
        else
        {
          $("#result").html("<p>an error occured ...</p>");
        }
      });
    };
  });
</script>
|]

addDocs :: LT.Text
addDocs =
  -- generate html with hamlet
  (renderHtml . defaultLayout $ [xshamlet|
<form>
  <textarea .span6 name=document #txt-document style=height:100px>
    [{
    "uri": "joke://joke1",
    "description": {
      "wer": "Adi Furler",
      "was": "Schöne Bilder aus Bremen. Aber eins verstehe ich nicht: Wieso singen die eigentlich \"We want the cup\", die haben den Pokal doch schon?",
      "wo": "im Sportstudio nach einem Bericht über eine Pokalsiegerfeier in Bremen, bei der Wynton Rufer und die Fans \"We won the cup!\" sangen",
      "gruppe": "Reportersprüche"
    },
    "index": {
      "wer": {
        "content": "Adi Furler"
      },
      "was": {
        "content": "Schöne Bilder aus Bremen. Aber eins verstehe ich nicht: Wieso singen die eigentlich \"We want the cup\", die haben den Pokal doch schon?"
      },
      "wo": {
        "content": "im Sportstudio nach einem Bericht über eine Pokalsiegerfeier in Bremen, bei der Wynton Rufer und die Fans \"We won the cup!\" sangen"
      },
      "gruppe": {
        "content": "Reportersprüche"
      }
    }
    }]
  <button .btn .btn-primary #btn-add>
    Add Document
|]) `LT.append`
  -- generate javascript
  renderJavascriptUrl (\_ _ -> "") [julius|
<script>
  $(document).ready(function() {
    $("#btn-add").click(function(ev){
      ev.preventDefault();
      var json = $("#txt-document").val();
      $.post( "/document/insert"
            , json
            , function(data) {
                if (data.code === 0) alert ("Document added to Index")
                else alert ("Error occured")
              }
            );
    });
  });
</script>
|]

-- | default layout
defaultLayout :: ToMarkup a => a -> Markup
defaultLayout content = [xshamlet|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Holumbus Server
    <script src="http://code.jquery.com/jquery-1.10.0.min.js">
    <script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js" type="text/javascript">
    <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet">
  <body>
    <div .navbar .navbar-fixed-top>
      <div .navbar-inner>
        <div .container>
          <a .brand href="/">Holumbus Server
          <ul .nav .nav-tabs>
            <li>
              <a href="/">Search
            <li>
              <a href="/add">Add Documents
    <div .container style="margin-top:70px">
      #{content}
|]
