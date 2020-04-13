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
var ols = require ('ol/source');
var olp = require ('ol/proj');
var olst = require ('ol/style');
var olg = require ('ol/geom');

// The projection we are using
var projection = 'EPSG:3857';

// Create a map and add it to a DOM element and center it around a longitude and latitude
// and set initial zoom
exports.createMapImpl = function (element,lon, lat, z) {
  return function() {
    return new ol.Map({
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
};

// Adjust the center of the map around the logitude and latitude.
exports.setCenterImpl = function (map, lon, lat) {
  return function () {
    var v = map.getView();
    v.setCenter (olp.fromLonLat([lon, lat]));
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

// Gets the current coordinates
exports.getCoordinatesImpl = function (just, nothing, geo) {
  return function () {
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
    var c = { longitude: px, latitude: py, accuracy: pax, altitude: hx, altitudeAccuracy: hax };
    return (c);
  }
}

//
// Functions for openlayers geolocation added to a map with a cursor
//

// Update the geometry when the accuracy changes
function changeAccuracyGeometry (geo, accuracyFeature) {
  return function () {
    var g = geo.getAccuracyGeometry();
    accuracyFeature.setGeometry(g);
  }
}

// Update the position when it changes
function changePosition (geo, positionFeature) {
  return function() {
    var coordinates = geo.getPosition();
    positionFeature.setGeometry(coordinates ?
      new olg.Point(coordinates) : null);
  }
}

// Initiate the geolocation mapping
exports.addGeolocationToMapImpl = function (map) {
  return function () {

    // Create the geolocation device
    var v = map.getView();
    var geo = new ol.Geolocation({
      trackingOptions: {
        enableHighAccuracy: true
      },
      projection: v.getProjection()
    });

    // handle geolocation error.
    geo.on('error', function(error) {
      console.log (error);
    });

    // Adjust to change on accuracy
    var accuracyFeature = new ol.Feature();
    geo.on('change:accuracyGeometry', changeAccuracyGeometry(geo, accuracyFeature));
    
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
    geo.on('change:position', changePosition (geo, positionFeature));

    // Add a vector layer whith these features
    new oll.Vector({
      map: map,
      source: new ols.Vector({
        features: [accuracyFeature, positionFeature]
      })
    });

    // Return with the geolocator
    return geo;
  }
}
