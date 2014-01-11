module Holumbus.Server.Template
( index
, addDocs
) where
import qualified Data.Text.Lazy as LT

import           Text.Hamlet
import           Text.Julius
import           Text.Blaze.Html.Renderer.Text    (renderHtml)
import           Text.Blaze                       (Markup, ToMarkup)

-- | main page
index :: Int -> LT.Text
index _docs =
  -- generate html with hamlet
  (renderHtml . defaultLayout $ [xshamlet|
<h1>
  Search Documents
<hr>
<div .row>
  <div .span8>
    <form>
      <div .input-append>
        <input .input-xxlarge type=text #txt-search>
        <button .btn .btn-primary type=button #btn-search>Search
      <hr>
      <p #result-count>
      <table .table .table-bordered .table-striped .table-condensed>
        <thead>
          <tr>
            <th>URI
            <th colspan=2>Document
            <th>
        <tbody #result-body>
          <tr>
            <td colspan=4>
              No results.
      <div #paginator style="display:none;text-align:center">
        <div .pagination>
          <ul>
            <li #pagination-prev><a href="#">Prev</a>
            <li><a href="#" id="pagination-page">Page 1</a>
            <li #pagination-next><a href="#">Next</a>
  <div .span4>
     <h5>
       Help
     <table .table .table-condensed .table-bordered .table-striped>
       <tr>
         <td>case-sensitive query
         <td><strong>!</strong>chris<br />
            <strong>!</strong>Chris
       <tr>
         <td>fuzzy query
         <td><strong>~</strong>chris<br />
            <strong>~</strong>hcris
       <tr>
         <td>phrase query
         <td><strong>"</strong>this is a phrase<strong>"</strong>
       <tr>
         <td>brackets
         <td><strong>(</strong>...<strong>)</strong>
       <tr>
         <td>context-sensitive query
         <td>context<strong>:</strong>query<br />
            people<strong>:</strong>chris<br />
            developer<strong>,</strong>students<strong>,</strong>people:chris
       <tr>
         <td>query combinators
         <td>AND, OR, NOT, BUT
       <tr>
         <td>query boosting
         <td>toAscList<strong>^</strong>1.5 OR toList

|]) `LT.append`
  -- generate javascript
  renderJavascriptUrl (\_ _ -> "") [julius|
<script>
  $(document).ready(function() {

    var globalPage = 1;
    var globalPerPage = 5;
    var globalPages = undefined;
    var globalResults = undefined;
    var globalCompletionMax = 20;

    $(document).keypress(function(event) {
      if ( event.which == 13 ) {
         event.preventDefault();
         globalPage = 1;
         search(event);
      }
    });

    // auto completion for search box
    // XXX should be improved or removed
    $("#txt-search").typeahead({
      source: function(query, callback) {
        $.get("/completion/" + query + "/" + globalCompletionMax, function(data) {
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

    // search button handler
    $("#btn-search").click(function(e){
      globalPage = 1;
      search(e);
    });

    $("#pagination-prev").click(function(e) {
      if (globalPage > 1)
      {
         globalPage--;
         search(e);
      }
    });

    $("#pagination-next").click(function(e) {
      if (globalPage < globalPages)
      {
         globalPage++;
         search(e);
      }
    });

    // handles simple search responses without paging
    var simpleSearchCompletedHandler = function(data) {
        if (data.code === 0)
        {
          var docs = data.msg;
          if (docs.length === 0)
          {
            $("#result-body").html("<tr><td colspan=\"4\">No results.</td></tr>");
            return false;
          }

          var res = "";
          $(docs).each(function(i,e) {
             var desc = e.desc
             var first = true;
             var props = Object.keys(desc).length;
             for (var key in desc){
               res += "<tr>";
               if (first)
               {
                 res += "<td rowspan=\"" + props +"\">" + e.uri + "</td>";
               }
               res += "<th>" + key + "</th><td>" + desc[key] + "</td>>";
               if (first)
               {
                 res += "<td rowspan=\"" + props + "\">";
                 res += "<button class\"btn btn-alert remove\">-</button></td>"
               }
               res += "</tr>";
               first = false;
             }
          });
          $("#result-body").html(res);
        }
        else
        {
           alert("Error occurred - please check server!");
        }
    };

    // handles search results with paging
    var pagedSearchCompletedHandler = function(data) {
        if (data.code === 0)
        {
          var pager = data.msg;
          globalPage    = pager.offset / pager.max + 1;
          globalPerPage = pager.max;
          globalResults = pager.count;
          globalPages   = Math.max(Math.ceil(globalResults / globalPerPage),1);
          simpleSearchCompletedHandler({code:0, msg:pager.result});

          $("#result-count").html(globalResults + " documents found");
          $("#pagination-next").removeClass("disabled");
          $("#pagination-prev").removeClass("disabled");
          $("#pagination-page").text("Page " + globalPage + " of " + globalPages);
          if (globalPage === 1)
          {
             $("#pagination-prev").addClass("disabled");
          }
          if (globalPage === globalPages)
          {
             $("#pagination-next").addClass("disabled");
          }
	  $("#paginator").show();
        }
        else
        {
           alert("Error occurred - please check server!");
        }
    };


    // search handler posts query to api and handles results
    var search = function(ev){
      ev.preventDefault();
      var query = $("#txt-search").val();

      if (query === "") {
         $("#result-body").html("<tr><td colspan=\"4\">No results.</td></tr>");
         return false;
      }
      $.get("/search/" + query + "/" + ((globalPage-1) * globalPerPage) + "/" + globalPerPage, pagedSearchCompletedHandler);
     };
  });
</script>
|]

addDocs :: LT.Text
addDocs =
  -- generate html with hamlet
  (renderHtml . defaultLayout $ [xshamlet|
<h1>
  Add Documents
<hr>
<form>
  <div .row>
    <div .span7>
      <table .table .table-condensed #entity-table>
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
            <input type="text" .persistent-value value="sample description" />
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
            <input type="text" .indexed-value value="sample index information" />
          <td>

    <div .span5>
      <textarea name=document #txt-document style=height:400px;width:100%>
      <button .btn .btn-primary #btn-add style=float:right>
        Add
|]) `LT.append`
  -- generate javascript
  renderJavascriptUrl (\_ _ -> "") [julius|
<script>
  $(document).ready(function() {

    // updates the json representation of apiDoc
    updateApiDoc = function(ev) {
      // empty api Document
      var apiDoc = {
        "uri": $("#uri").val(),
        "description" : {},
        "index": {}
      };

      // add entity properties to description
      var values = $(".persistent-value");
      $(".persistent-key").each(function(i,v) {
        eval("apiDoc.description['"+$(v).val()+"'] ='"+$(values[i]).val()+"'");
      });

      // add indexed information to index
      values = $(".indexed-value");
      $(".indexed-key").each(function(i,v) {
        eval("apiDoc.index['"+$(v).val()+"'] = '"+$(values[i]).val()+"'");
      });

      // wrap doc into list and format as json
      var apiDocs = [apiDoc];
      $("#txt-document").html(JSON.stringify(apiDocs));
    };
    updateApiDoc();
    $(document).on("change", "#entity-table * input", updateApiDoc);

    // add another entity property to editor
    $("#add-persistent").on("click", function(ev) {
      ev.preventDefault();
      $(".persistent").after("<tr><td></td>" +
        "<td><input class=\"persistent-key\" type=\"text\"/></td>" +
        "<td><input class=\"persistent-value\" type=\"text\" /></td>" +
        "<td></td></tr>");
    });

    // add another context to indexed information
    $("#add-index").on("click", function(ev) {
      ev.preventDefault();
      $(".indexed").after("<tr><td></td>" +
        "<td><input class=\"indexed-key\" type=\"text\"/></td>" +
        "<td><input class=\"indexed-value\" type=\"text\" /></td>" +
        "<td></td></tr>");
    });

    // add document handler posts apiDocs to api
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
    <script src="http://code.jquery.com/jquery-1.10.2.min.js">
    <script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js" type="text/javascript">
    <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet">
  <body>
    <div .navbar .navbar-fixed-top>
      <div .navbar-inner>
        <div .container>
          <a .brand href="/">Holumbus Server
          <ul .nav .nav-tabs>
            <li>
              <a href="/search">Search
            <li>
              <a href="/add">Add
            <li>
              <a href="https://github.com/ulfs/holumbus" target="git" >Help
    <div .container style="margin-top:70px">
      #{content}
|]
