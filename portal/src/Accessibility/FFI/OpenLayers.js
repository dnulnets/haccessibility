//
// The Openlayers API mapping for purescript.
//
// This is just a very crude mapping and only helps out with what I need for this application. It is no
// complete mapping.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');
var oll = require ('ol/layer');
var olc = require ('ol/control')
var ols = require ('ol/source');
var olp = require ('ol/proj');
var olst = require ('ol/style');
var olg = require ('ol/geom');
var olgp = require ('ol/geom/Polygon');
var oli = require ('ol/interaction');

// The projection we are using
var projection = 'EPSG:3857';

// If we are running a mock GPS
var mock = false;
const mock_lat = 62.391409; // Storgatan in Sundsvall
const mock_lon = 17.304742;
const mock_accuracy = 10.0; // 10 meters accuracy

// Our controls
var AddItemControl = (function (Control) {

  function AddItemControl(opt_options) {
    
    var options = opt_options || {};

    var button = document.createElement('button');
    button.innerHTML = 'A';
    button.id = "map-add-item";
    var element = document.createElement('div');
    element.className = 'map-add-item ol-unselectable ol-control';
    element.appendChild(button);

    Control.call(this, {
      element: element,
      target: options.target
    });
  }

  if ( Control ) AddItemControl.__proto__ = Control;
  AddItemControl.prototype = Object.create( Control && Control.prototype );
  AddItemControl.prototype.constructor = AddItemControl;

  return AddItemControl;

} (olc.Control));

var RefreshControl = (function (Control) {

  function RefreshControl(opt_options) {
    
    var options = opt_options || {};

    var button = document.createElement('button');
    button.innerHTML = 'R';
    button.id = "map-refresh";
    var element = document.createElement('div');
    element.className = 'map-refresh ol-unselectable ol-control';
    element.appendChild(button);

    Control.call(this, {
      element: element,
      target: options.target
    });
  }

  if ( Control ) RefreshControl.__proto__ = Control;
  RefreshControl.prototype = Object.create( Control && Control.prototype );
  RefreshControl.prototype.constructor = RefreshControl;

  return RefreshControl;

} (olc.Control));

var CenterControl = (function (Control) {

  function CenterControl(opt_options) {
    
    var options = opt_options || {};

    var button = document.createElement('button');
    button.innerHTML = 'C';
    button.id = "map-center";
    var element = document.createElement('div');
    element.className = 'map-center ol-unselectable ol-control';
    element.appendChild(button);

    Control.call(this, {
      element: element,
      target: options.target
    });
  }

  if ( Control ) CenterControl.__proto__ = Control;
  CenterControl.prototype = Object.create( Control && Control.prototype );
  CenterControl.prototype.constructor = CenterControl;

  return CenterControl;

} (olc.Control));

exports.createSelect = function () {
  return new oli.Select();
}

exports.addInteractionImpl = function (i, self)
{
  console.log ("addInteraction");
  return function () {
    console.log ("addInteraction Effect");
    self.addInteraction (i);
  }
}

// Create a map and add it to a DOM element and center it around a longitude and latitude
// and set initial zoom
exports.createMapImpl = function (element,lon, lat, z) {
  return function() {

    return new ol.Map({
      controls: olc.defaults().extend([
        new AddItemControl(),
        new RefreshControl(),
        new CenterControl()
      ]),
      target: element,
      layers: [
        new oll.Tile({
          source: new ols.OSM()
        })
      ],
      view: new ol.View({
        projection: projection ,
        center: [lon, lat],
        center: olp.fromLonLat([lon, lat], projection),
        zoom: z
      })            
    });
  }
}

// Adjust the center of the map around the logitude and latitude.
exports.setCenterImpl = function (map, lon, lat) {
  return function () {
    var v = map.getView();
    v.setCenter (olp.fromLonLat([lon, lat], projection));
  }
}

// Removes the target from the map, so it does not display anymore and can be
// garbage collected.
exports.removeTargetImpl = function (map) {
  return function () {
      map.setTarget (undefined);
  }
}

// Sets the tracking on or off
exports.setTrackingImpl = function (geo, onoff) {
  return function () {
    geo.setTracking (onoff);  
  }
}

// Converts coordinates
function convertCoordinate (just, nothing, geo)
{
  if (mock == false) {
    var px, py, pax, hx,hax
    var p = geo.getPosition();
    if (p==null) {
      px = nothing
      py = nothing
    } else {
      p = olp.toLonLat (p, geo.getProjection());
      px = just(p[0])
      py = just(p[1])
    }

    var pa = geo.getAccuracy();
    if (pa==null) {
      pax = nothing;
    } else {
      pax = just(pa);
    }
    var h = geo.getAltitude();
    if (h==null) {
      hx = nothing;
    } else {
      hx = just(h);
    }
    var ha = geo.getAltitudeAccuracy();
    if (ha==null) {
      hax = nothing;
    } else {
      hax = just(ha);
    }
    return ({ longitude: px, latitude: py, accuracy: pax, altitude: hx, altitudeAccuracy: hax });
  } else {
    return ({ longitude: just(mock_lon), latitude: just(mock_lat), accuracy: just(mock_accuracy), altitude: nothing, altitudeAccuracy: nothing});
  }
}

// Gets the current coordinates asynchronous
exports._getCoordinateImpl = function (just, nothing, geo) {

  return function (onError, onSuccess) {

    geo.once ('change:position', function () {
      onSuccess (convertCoordinate(just, nothing, geo));
    });

    return function (cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  }
}

// Gets the current coordinates
exports.getCoordinateImpl = function (just, nothing, geo) {
  return function () {
    return (convertCoordinate(just, nothing, geo));
  }
}

//
// Functions for openlayers geolocation added to a map with a cursor
//

// Update the geometry when the accuracy changes
function changeAccuracyGeometry (geo, accuracyFeature) {
  return function () {
    var g;
    if (mock == false) {
      g = geo.getAccuracyGeometry();
      accuracyFeature.setGeometry(g);
    } else {
      g = new olg.Circle (olp.fromLonLat([mock_lon, mock_lat], projection), mock_accuracy);
      accuracyFeature.setGeometry(g);
    }
  }
}

// Update the position when it changes
function changePosition (geo, positionFeature) {
  return function() {
    if (mock == false) {
      var coordinates = geo.getPosition();
      positionFeature.setGeometry(coordinates ? new olg.Point(coordinates) : null);
    } else {
      positionFeature.setGeometry(new olg.Point(olp.fromLonLat([mock_lon, mock_lat], mock_accuracy)));
    }
  }
}

// Initiate the geolocation mapping
exports.addGeolocationToMapImpl = function (map) {
  return function () {

    // Create the geolocation device
    var geo = new ol.Geolocation({
      trackingOptions: {
        enableHighAccuracy: true
      },
      projection: map.getView().getProjection()
    });

    // handle geolocation error.
    geo.on('error', function(error) {
      console.log (error);
    });

    // Adjust to change on accuracy
    var accuracyFeature = new ol.Feature();
    geo.on('change:accuracyGeometry', changeAccuracyGeometry(geo, accuracyFeature));
    accuracyFeature.setProperties ({ "guid": "guid-accuracy"});
    
    var positionFeature = new ol.Feature();
    positionFeature.setStyle(new olst.Style({
      image: new olst.Circle({
        radius: 6,
        fill: new olst.Fill({
          color: '#3399CC'
        }),
        stroke: new olst.Stroke({
          color: '#fff',
          width: 2
        })
      })
    }));
    positionFeature.setProperties ({ "guid": "guid-position"});

    geo.on('change:position', changePosition (geo, positionFeature));

    // Add a vector layer whith these features
    var vl = new oll.Vector({
      source: new ols.Vector({
        features: [accuracyFeature, positionFeature]
      })
    });
    vl.setProperties({ "guid": "guid-gps"});

    // Add the layer to the map
    map.addLayer(vl);

    // Return with the geolocator
    return geo;
  }
}

//
// Styles for the point of interests
//

// Circle, size 6, green and a black circle
const circlePOI = new olst.Circle({
  radius: 6,
  fill: new olst.Fill({color: "#32CD32"}),
  stroke: new olst.Stroke({color: "#000000",width: 2
  })
})

const circlePOIWeather = new olst.Circle({
  radius: 6,
  fill: new olst.Fill({color: "#0080FF"}),
  stroke: new olst.Stroke({color: "#000000",width: 2
  })
})

// Circle of search area, size 6, green and a black circle
const circlePOISearch = new olst.Circle({
  radius: 6,
  fill: new olst.Fill({color: "#32CD32"}),
  stroke: new olst.Stroke({color: "#000000",width: 2
  })
})

// Text, must be updated depending on the name of the feature
function textPOI (name) {
  return new olst.Text({
    text: name,
    offsetY: 15,
    font: "12px Calibri, sans-serif"
  });
}

// The final POI style
function stylePOI (feature, resolution) {
  var ip = circlePOI; // Local POI
  if (feature.get("type") == 2) // Weather POI
    ip = circlePOIWeather;
  return new olst.Style ({
    image: ip,
    text: textPOI (feature.get("name"))
    });
}

// Create the POI layer
exports.createPOILayerImpl = function (npoi, lon, lat, d, pois) {

  return function () {

    // Create a feature that has the distance and lo/la of the search area
    var circle = new olg.Circle (olp.fromLonLat([lon, lat], projection), d);
    var searchArea = new ol.Feature(circle);
    searchArea.setStyle (new olst.Style ({
        stroke: new olst.Stroke({
          color: '#000',
          width: 1,
          lineDash: [4,8],
        })
      }));

    // Create a feature that has the Case 3 area marked
    
    // Create the list of features
    var i;
    var lofFeatures = new Array (pois.length+1);
    for (i = 0; i<pois.length; i++) {
      lofFeatures[i] = new ol.Feature({name: pois[i].name, type: npoi (pois[i].type),
        geometry: new olg.Point(olp.fromLonLat([pois[i].longitude, pois[i].latitude], projection))});
    }
    lofFeatures[pois.length] = searchArea;

    // Add the features to a layer
    var v = new oll.Vector ({
      source: new ols.Vector({
        features: lofFeatures
      })
    });
    v.setStyle (stylePOI);
    v.setProperties({ "guid": "guid-poi"});

    return v;
  }
}

// Add a layer to the map
exports.addLayerToMapImpl = function (map, layer) {
  return function () {
    map.addLayer (layer);
  }
}

// Remove a layer from the map
exports.removeLayerFromMapImpl = function (map, layer) {
  return function () {
    map.removeLayer (layer);
  }
}

// Sets the test mode for the GPS-locator, then we always ends at mock position.
exports.setTestModeImpl = function (map, b) {

  return function() {

    // Find the GPS layer
    mock = b;
    if (b == true) {
      var lcoll = map.getLayers();
      lcoll.forEach(function (l) {
        if (l.getProperties().guid == "guid-gps") {
          l.getSource().forEachFeature(function(feature) {
              var fp = feature.getProperties();
              if (fp.guid == "guid-position") {
                feature.setGeometry(new olg.Point(olp.fromLonLat([mock_lon, mock_lat], projection)));                
              }
              if (fp.guid == "guid-accuracy") {
                feature.setGeometry(new olg.Circle (olp.fromLonLat([mock_lon, mock_lat], projection), mock_accuracy));        
              }
          });
        }
      });
    }
  }
}
