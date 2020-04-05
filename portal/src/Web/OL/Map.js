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

// Create a map to test with
exports.createMap = function (element) {
  return function (lon) {
    return function (lat) {
      return function (z) {
        return function() {
          return new ol.Map({
            target: element,
            layers: [
              new oll.Tile({
                source: new ols.OSM()
              })
            ],
            view: new ol.View({
              center: olp.fromLonLat([lon, lat]),
              zoom: z
            })            
          });
        }
      }
    }
  } 
};

// Adjust the center of the map
exports.setCenter = function (map) {
  return function (lon) {
    return function (lat) {
      return function () {
        var v = map.getView();
        v.setCenter (olp.fromLonLat([lon, lat]));
      }
    }
  }
}

// Removes the target from the map, so it does not display anymore and can be
// garbage collected
exports.removeTarget = function (map) {
  return function () {
      map.setTarget (undefined);
  }
}

// Sets the tracking on or off
exports.setTracking = function (geo) {
  return function (onoff) {
    return function () {
      geo.setTracking (onoff);  
    }
  }
}

// Update the geometry of the accuracy on change
function changeAccuracyGeometry (geo, accuracyFeature) {
  return function () {
    var g = geo.getAccuracyGeometry();
    accuracyFeature.setGeometry(g);
  }
}

// Update the position on change
function changePosition (geo, positionFeature) {
  return function() {
    var coordinates = geo.getPosition();
    positionFeature.setGeometry(coordinates ?
      new olg.Point(coordinates) : null);
  }
}

// Initiate the geolocation mapping
exports.addGeolocationToMap = function (map) {
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
