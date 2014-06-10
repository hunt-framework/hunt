-- ----------------------------------------------------------------------------
{- |
  The main Hunt front-end template.
-}
-- ----------------------------------------------------------------------------

module Hunt.Server.Template
  ( index
  , quickstart
  )
where

import qualified Data.Text.Lazy as LT

import           Text.Hamlet
import           Text.Julius
import           Text.Blaze.Html.Renderer.Text    (renderHtml)
import           Text.Blaze                       (Markup, ToMarkup)

-- ------------------------------------------------------------

-- | Main page.
index :: Int -> LT.Text
index _docs =
  -- generate html with hamlet
  (renderHtml . defaultLayout $ [xshamlet|
<div .page-header>
  <h1>Search Documents</h1>
<div .row>
  <div .col-xs-8>
      <div .input-group>
        <input type=text .form-control #txt-search>
        <span .input-group-btn>
          <button #btn-search .btn .btn-default type=button>Search
      <hr>
      <ul .nav .nav-tabs>
        <li .active>
          <a href="#" #result-docs data-toggle=tab>Documents
        <li>
          <a href="#" #result-map data-toggle=tab>Map
        <li .disabled>
          <a href="#">
            <span #result-count .label .label-primary>

      <div .tab-content>
        <div .tab-pane .active #doc-results>
          <table .table .table-bordered .table-striped .table-condensed>
            <thead>
              <tr>
                <th>URI
                <th>Score
                <th colspan=2>Document
            <tbody #result-body>
              <tr>
                <td colspan=4>
                  No results.
        <div .tab-pane #map-results>
          <div #map style="width:100%; min-height:400px">
      <div #paginator style="display:none;text-align:center">
        <ul .pagination>
          <li #pagination-prev><a href="#">Prev</a>
          <li><a href="#" id="pagination-page">Page 1</a>
          <li #pagination-next><a href="#">Next</a>
  <div .col-xs-4>
    #{examplesWidget}
<br />
|]) `LT.append`
  -- generate javascript
  renderJavascriptUrl (\_ _ -> "") [julius|
<script>
  $(document).ready(function() {

    var globalPage = 1;
    var globalPerPage = 20;
    var globalPages = undefined;
    var globalResults = undefined;
    var globalCompletionMax = 20;
    var snippet_no_results = "<tr><td colspan=\"4\">No results.</td></tr>";
    var map;
    var mapPos = [20,0]
    var mapZoom = 2;

    var initMap = function() {
         if (map !== undefined)
         {
            mapPos = map.getCenter();
            mapZoom = map.getZoom();
            map.remove();
         }
         map = L.map('map').setView(mapPos, mapZoom);
         L.tileLayer('https://{s}.tiles.mapbox.com/v3/{id}/{z}/{x}/{y}.png', {
            maxZoom: 18,
            attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, ' +
                         '<a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
                         'Imagery © <a href="http://mapbox.com">Mapbox</a>',
            id: 'examples.map-i86knfo3'
         }).addTo(map);
    }
    initMap();

    $("#result-docs").click(function(ev) {
      ev.preventDefault();
      $("#map-results").hide();
      $("#doc-results").show();
    });
    $("#result-map").click(function(ev) {
      ev.preventDefault();
      $("#map-results").show();
      $("#doc-results").hide();
      initMap();
      search(ev);
    });

    $(document).keypress(function(event) {
      if ( event.which == 13 ) {
         event.preventDefault();
         globalPage = 1;
         search(event);
      }
    });

    // auto completion for search box
    // TODO: should be improved or removed
    $("#txt-search").typeahead({
      source: function(query, callback) {
        $.get("/completion/" + encodeURIComponent(query) + "/" + globalCompletionMax, function(data) {
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
            $("#result-body").html(snipped_no_results);
            return false;
          }
          initMap();
          var res = "";
          $(docs).each(function(i,doc) {
             var score = doc.score !== undefined ? doc.score : 0;
             var desc = doc.description;
             var first = true;
             var props = Object.keys(desc).length;
             for (var key in desc){
               res += "<tr>";
               if (first) {
                 res += "<td rowspan=\"" + props +"\">" + doc.uri + "</td>";
                 res += "<td rowspan=\"" + props +"\">" + score + "</td>";
               }
               res += "<th>" + key + "</th><td>" + desc[key] + "</td>>";
               res += "</tr>";
               first = false;
               if (key === "location") {
                  var pos = desc[key].split(" ");
                  L.marker([pos[0], pos[1]]).addTo(map).bindPopup(doc.uri);
               }
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
          pagerMax = pager.max;
          if (pagerMax < 0) { pagerMax = 1000000 };
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
         $("#result-body").html(snippet_no_results);
         return false;
      }
      $.get("/search/" + encodeURIComponent(query) + "/" + ((globalPage-1) * globalPerPage) + "/" + globalPerPage, pagedSearchCompletedHandler);
     };
  });
</script>
|]

quickstart :: LT.Text
quickstart =
  (renderHtml . defaultLayout $ [xshamlet|
<div .page-header>
  <h1>Quickstart&nbsp;&nbsp;
    <small>Query Interface
<h2>
  1. About Contexts
<div .row>
  <div .col-xs-12>
    <h3>Contexts
    <p>The term context has already been introduced in the Holumbus framework. It isused to classify and distinguishes indexed information.
    <p>In an analogy to databases, a context could be seen as a table. It has a unique name and a configured behaviour. The configuration is made with the help of the context schema.
    <p>Documents are always indexed with respect to at least one context. It is also possible to create and remove contexts during runtime.
    <p>Each context is created with an individual schema.
    <h3>Context Schema
    <p>Each context has a corresponding context schema. The schema defines the contexts behaviour. That includes:
    <ul>
      <li>Definition of how the input should be split up into words.
      <li>Definition of how these words should be preprocessed and normalized.
      <li>Definition of the index type to use (text, date, geolocation, ...).
    <h3>Context Types
    <p>Each index implementation is integrated into the search engine as a context type.
    <p>The search engine is started with a list of valid types. Only a context schema created with one of those valid types is a valid schema.
    <p>The context type contains information how the index implementation should be used.
    <p>This includes:
    <ul>
      <li>
        <strong>Splitting Words
        When inserting a big text into a context, some meta information is required, how this large text should be split up into words. The context type gives a default configuration how this task should be handled. The default should fit in most cases, else it could be overwritten by the context schema.
      <li>
        <strong>Validation
        Each implementation of an index-structure handles data differently. A basic validation of the index key needs to be performed before insertion. For example, a numeric context type needs to make sure each key is a valid number.
<h2>
  2. Example
<div .row>
  <div .col-xs-12>
     <h3>Scenario Introduction
     <p>Consider the maintaining of a blog-like news website. Each blog entry contains a subject, the date of publication, the content and the location, where the news happened.
     <p>The page has become rather large and it is necessary to provide a way to search and filter the articles.
     <h3>Context Creation
     <p>The first step, when building a Hunt search engine application, is to think about the necessary contexts. There are four fields of information stored for an article. The publishing date should be a context by itself and is a perfect match for the date context type. The location could be stored in location contexts. The obvious choice for the articles’ content is the text type. One could use a single context for the subject and the content.
     <p>In this case it is a more flexible approach, to keep the subject and content separate. By doing so, it is possible to assign a higher weight to the subject. This means, that the document in the result set gets a ranking boost, if a search term occurs in the subject line. A blog entry “Introduction to Functors” would rank higher in a search for “functor” than another blog entry, where the word occurs just in the content itself.
     <p>First, let us create the four contexts:
<div .row>
  <div .col-xs-10>
     <textarea .input .json style="width:100%; height:200px" data-query="contexts">
        [{"cmd": "insert-context", "context": "content", "schema": {"type": "text"}}
        ,{"cmd": "insert-context", "context": "subject", "schema": {"type": "text", "weight":2}}
        ,{"cmd": "insert-context", "context": "publish_date", "schema": {"type": "date", "default": false}}
        ,{"cmd": "insert-context", "context": "location", "schema": {"type": "position", "default": false}}
        ]
  <div .col-xs-2>
    <button .btn .btn-primary data-query=contexts>Execute
<div .row>
  <div .col-xs-12>
    <h3>Inserting Documents
    <p>Now that all required contexts are created, documents can be inserted. Each document needs to be inserted with an unique identifier. In this example, it makes sense to use the URL of an article to identify it, a database ID might be reasonable as well. While the framework helps with basic word splitting and normalization, there is still some work to do.
    <p>The actual article needs to be transformed into the ApiDocument data structure, before it can be inserted.
    <p>The index object defines the input for each context. Note that index and document description names do not have to match and not every document has to insert values to every context.
    <p>The description defines, which values should be stored alongside the document ID. For applications storing the document data separately, for example in a database, it is a good practice to leave the document description empty. This avoids redundancy and possible inconsistent data sets.
<div .row>
  <div .col-xs-10>
     <textarea .input .json style="width:100%; height:200px" data-query="documents">
       [{"cmd":"insert","document":{"uri":"Entry1","index":{"subject":"Something incredible happened in Paris","publish_date":"2013-07-17","content":"Ad eos audire dissentias. Rebum iusto offendit sit te, sonet accusam ei sed. Ceteros periculis imperdiet vim ex, te illum soluta pro. Vis alterum saperet feugait no, tamquam epicuri vituperatoribus nec ea, ex vel doctus democritum.","location":"48.858503-2.294809"},"description":{"subject":"Something incredible happened in Paris","content":"Ad eos audire dissentias. Rebum iusto offendit sit te, sonet accusam ei sed. Ceteros periculis imperdiet vim ex, te illum soluta pro. Vis alterum saperet feugait no, tamquam epicuri vituperatoribus nec ea, ex vel doctus democritum.","date":"2013-07-17","location":"48.858503 2.294809"}}},{"cmd":"insert","document":{"uri":"Entry2","index":{"subject":"Something incredible happened in New York","publish_date":"2012-07-17","content":"Ridens essent ex eam. No viris facete admodum mei, ut lorem iuvaret duo. Augue appellantur dissentiunt cum ut, te vel cibo semper, quod impedit eam in. Id duo sanctus facilisis, an postea delicatissimi eam, fuisset forensibus pro ne. Id omnis dicam intellegebat quo, ne admodum consulatu aliquando has, impedit comprehensam sit te.","location":"40.722673--73.995008"},"description":{"subject":"Something incredible happened in New York","content":"Lorem ipsum dolor sit amet, apeirian consequat in pri, an enim falli fabulas usu. Ei mei nonumy eligendi qualisque, feugiat intellegat duo ut. Cu per brute nostro. Odio dicam nec at, everti interpretaris ea duo. Ne cum delicata assueverit. Eu vix malis voluptatibus, no debet numquam his, inimicus adversarium ne vix. Aliquando deterruisset mei cu.","date":"2012-07-17","location":"40.722673 -73.995008"}}},{"cmd":"insert","document":{"uri":"Entry3","index":{"subject":"Something incredible happened in Australia","publish_date":"2011-07-17","content":"Lorem ipsum dolor sit amet, apeirian consequat in pri, an enim falli fabulas usu. Ei mei nonumy eligendi qualisque, feugiat intellegat duo ut. Cu per brute nostro. Odio dicam nec at, everti interpretaris ea duo. Ne cum delicata assueverit. Eu vix malis voluptatibus, no debet numquam his, inimicus adversarium ne vix. Aliquando deterruisset mei cu.","location":"-33.890937-151.085129"},"description":{"subject":"Something incredible happened in Australia","content":"Ridens essent ex eam. No viris facete admodum mei, ut lorem iuvaret duo. Augue appellantur dissentiunt cum ut, te vel cibo semper, quod impedit eam in. Id duo sanctus facilisis, an postea delicatissimi eam, fuisset forensibus pro ne. Id omnis dicam intellegebat quo, ne admodum consulatu aliquando has, impedit comprehensam sit te.","date":"2011-07-17","location":"-33.890937 151.085129"}}}]
  <div .col-xs-2>
    <button .btn .btn-primary data-query=documents>Execute
<div .row>
  <div .col-xs-12>
     <h3>Executing Queries
     <p>After indexing the blog articles, the index is ready for queries. Queries can be expressed as a Haskell data type, a JSON structure or as plain text. This section introduces each way with the help of a common query.
     <p>The blog search engine stores the publishing date of the articles in a separate context. A common use of this is to search for articles within a time frame. This can be done with a range query. It can be written as a plain text query in the following way:
     <p>publish_date:[2014-01-10 TO 2014-01-23]
     <p>Try it out in the search tab!
     <p>Range queries generally yield the best results (and impose less performance overhead), when applied to a specific context. This example query searches for documents published between 10th and 23rd January 2014.
     <p>The same query would look like this in JSON:
     <textarea .input .json style="width:100%;height:200px">
       [{"cmd":"search","max":10,"offset":0,"query":{"type":"context","contexts":["publish_date"],"query":{"type":"range","upper":"2014-01-23","lower":"2014-01-10"}}}]
<br />
<br />
<br />
|])`LT.append`
  -- generate javascript
  renderJavascriptUrl (\_ _ -> "") [julius|
<script>
  $(document).ready(function() {
    var format = function(ev) {
      try {
        var obj = eval($(this).val());
        $(this).val(JSON.stringify(obj, undefined, 2));
      } catch (e) {
        console.log(e);
      }
    };
    $("button[data-query]").click(function(ev) {
      ev.preventDefault();
      var query = $(this).attr("data-query");
      var json = $("[data-query='"+query+"']").val();
      $.ajax({
        url: "http://localhost:3000/eval",
        data: json,
        type: 'POST'
      });
    });

    $(".json").keyup(format);
    $(".json").each(format);
  });
</script>
|]


-- | template snippet for widget containing query examples
examplesWidget :: Markup
examplesWidget = [xshamlet|
<div .panel .panel-primary>
  <div .panel-heading>
    Help
  <div .panel-body>
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
         <td>AND, OR, AND NOT
       <tr>
         <td>query boosting
         <td>toAscList<strong>^</strong>1.5 OR toList
       <tr>
         <td>range query
         <td><strong>[</strong> 2014-02-10 <strong>TO</strong> 2012-02-16 <strong>]</strong>
|]


-- | default layout
defaultLayout :: ToMarkup a => a -> Markup
defaultLayout content = [xshamlet|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Hunt Server
    <link rel="stylesheet" href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">
    <link rel="stylesheet" href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css">
    <link rel="stylesheet" href="http://cdn.leafletjs.com/leaflet-0.7.2/leaflet.css" />
  <body role=document>
    <div .navbar .navbar-default .navbar-fixed-top role=navigation>
      <div .container>
        <div .navbar-header>
          <button type=button .navbar-toggle data-toggle=collapse data-target=.navbar-collapse>
            <span .sr-only>Toggle navigation
            <span .icon-bar>
            <span .icon-bar>
            <span .icon-icon-bar>
          <a .navbar-brand href="#">Hunt Server
        <div .navbar-collapse .collapse>
          <ul .nav .navbar-nav>
            <li>
              <a href="/search">Search
            <li>
              <a href="/quickstart">Quickstart
            <li>
              <a href="https://github.com/hunt-framework" target="git" >Help
    <div .container style="margin-top:40px">
      #{content}
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"></script>
  <script src="//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>
  <script src="https://rawgit.com/bassjobsen/Bootstrap-3-Typeahead/master/bootstrap3-typeahead.min.js"></script>
  <script src="http://cdn.leafletjs.com/leaflet-0.7.2/leaflet.js"></script>
|]

-- ------------------------------------------------------------
