{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Applicative ((<$>))
import qualified Data.Text.Lazy      as LT
import           Text.Julius
import           Yesod
import           Yesod.Holumbus

-- main application state
data App = App
    { index :: Holumbus
    }

-- main application routes
mkYesod "App" [parseRoutes|
/ HomeR GET
/holumbus/ HolumbusR Holumbus index
|]

-- typeclass the main application implements
instance Yesod App
instance YesodHolumbus App

-- application start
main :: IO ()
main = do
    app <- App <$> emptyIndex
    warp 3000 app

-- start page
-- JS needs some work, should be replaced by some
-- kind of JS client library for holumbus
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
   addScriptRemote $ "http://code.jquery.com/jquery-1.10.0.min.js"
   addScriptRemote $ "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
   addStylesheetRemote $ "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
   toWidget $ [julius|
  $(document).ready(function() {

    var globalPage = 1;
    var globalPerPage = 5;
    var globalPages = undefined;
    var globalResults = undefined;

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
        $.get("/holumbus/completion/" + query, function(data) {
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
          globalPage = pager.page;
          globalPerPage = pager.perPage;
          globalResults = pager.count;
          globalPages = Math.ceil(pager.count / pager.perPage);
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
      $.get("/holumbus/search/" + query + "/" + globalPage + "/" + globalPerPage, pagedSearchCompletedHandler);
     };
  });
|]
   [whamlet|
<html>
  <body>
    <div .container>
      <h1>HolumbusSearch Yesod-Subsite Example
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
                 <td><strong>!</strong>chris<br /><strong>!</strong>Chris
               <tr>
                 <td>fuzzy query
                 <td><strong>~</strong>chris <br /> <strong>~</strong>hcris
               <tr>
                 <td>phrase query
                 <td><strong>"</strong>this is a phrase<strong>"</strong>
               <tr>
                 <td>using brackets
                 <td><strong>(</strong>...<strong>)</strong>
               <tr>
                 <td>context-sensitive query
                 <td>context<strong>:</strong>query<br />people<strong>:</strong>chris
               <tr>
                 <td>mutli-context query
                 <td>developer<strong>,</strong>students<strong>,</strong>people:chris
               <tr>
                 <td>query combinators
                 <td>AND, OR, NOT, BUT
    |]


