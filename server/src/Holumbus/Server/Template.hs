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
<div .well  #result>
  no results...
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
  <h1>
    Add Documents
  <div .row>
    <div .span7>
      <table .table .table-condensed #entity>
        <tr>
          <th>URI
          <td colspan=3>
            <input type="text" .input-large #uri value="sampleId::1"/>
        <tr>
          <th>Entity
          <th>Property Key
          <th>Property Value
          <th>
            <button .btn .btn-success  #add-persistent>
              +
        <tr .persistent>
          <th>
          <td>
            <input type="text" .persistent-key value="sample key" />
          <td>
            <textarea .persistent-value>
              sample description
          <td>
        <tr>
          <th>Index
          <th>Context
          <th>Information
          <th>
            <button .btn .btn-success #add-index>
              +        
        <tr .indexed>             
          <th>
          <td>
            <input type="text" .indexed-key  value="sample context" />
          <td>
            <textarea .indexed-value>
              sample information
          <td>
          
    <div .span5>
      <textarea name=document #txt-document style=height:400px;width:100%>
      <button .btn .btn-primary #btn-add style=float:right>
        Add Documents
|]) `LT.append`
  -- generate javascript
  renderJavascriptUrl (\_ _ -> "") [julius|
<script>
  $(document).ready(function() {

    updateJson = function(ev) {
      var apiDoc = {
        "uri": $("#uri").val(),
        "description" : {},
        "index": {}
      };

      var values = $(".persistent-value");
      $(".persistent-key").each(function(i,v) {
        eval("apiDoc.description['"+$(v).val()+"'] ='"+$(values[i]).val()+"'"); 
      });

      values = $(".indexed-value");
      $(".indexed-key").each(function(i,v) {
        eval("apiDoc.index['"+$(v).val()+"'] = {'content':'"+$(values[i]).val()+"'}"); 
      });
    
      var apiDocs = [apiDoc]; 
      $("#txt-document").html(JSON.stringify(apiDocs));
    };
    updateJson();
   
    $(document).on("change", "#uri, .persistent-key, .persistent-value, .indexed-key, .indexed-value", updateJson);

    $("#add-persistent").on("click", function(ev) {
      ev.preventDefault();
      $(".persistent").after("<tr><td></td>" + 
        "<td><input class=\"persistent-key\" type=\"text\"/></td>" +
        "<td><textarea class=\"persistent-value\"></textarea></td>" + 
        "<td></td></tr>");
    });

    $("#add-index").on("click", function(ev) {
      ev.preventDefault();
      $(".indexed").after("<tr><td></td>" + 
        "<td><input class=\"indexed-key\" type=\"text\"/></td>" +
        "<td><textarea class=\"indexed-value\"></textarea></td>" + 
        "<td></td></tr>");
    });

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
