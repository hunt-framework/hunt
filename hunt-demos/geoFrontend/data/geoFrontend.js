function makeAutocomplete() {
    var cache = {};
    $( "#geoFrontend" ).autocomplete({
        minLength: 2,
        source: function( request, response ) {
            var term = request.term;
            if ( term in cache ) {
                response( cache[ term ] );
                return;
            }

            $.getJSON( "/autocomplete", request, function( data, status, xhr ) {
                cache[ term ] = data;
                response( data );
            });
        }
    });
}

function initMap() {
    // set up the map
    map = new L.Map('map');

    // create the tile layer with correct attribution
    var osmUrl='http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png';
    var osmAttrib='Map data © OpenStreetMap contributors';
    var osm = new L.TileLayer(osmUrl, {minZoom: 1, maxZoom: 18, attribution: osmAttrib});

    // start the map in South-East England
    map.setView(new L.LatLng((53.5681 + 53.5854) / 2, (9.6960 + 9.7496) / 2), 14);
    map.addLayer(osm);
}

var debugNoRange = false
var markers = []
function onSearch(){
    query = $("#geoFrontend")[0].value
    $.each(markers, function(i, marker){
         map.removeLayer(marker)
    })
    markers = []
    boundsString = ""

    if (!debugNoRange) {
        bounds = map.getBounds()
        ne = bounds.getNorthEast()
        sw = bounds.getSouthWest()
        minlat = Math.min(ne.lat, sw.lat)
        minlng = Math.min(ne.lng, sw.lng)
        maxlat = Math.max(ne.lat, sw.lat)
        maxlng = Math.max(ne.lng, sw.lng)
        boundsString = " position:[" + minlat + "-" + minlng + " TO " + maxlat + "-" + maxlng + "]"
    }

    var data = {term: query + boundsString}
    $.getJSON( "search", data, function( data ) {
        $.each(data, function(i, doc){
            var lat = doc.lat
            var lon = doc.lon
            var marker = L.marker([lat, lon]).addTo(map);
            markers.push(marker)
        })
    });
}


$().ready(function() {
    makeAutocomplete()
});

$(document).ready(function() {
    initMap()
    $("#submit").click(onSearch)
});
