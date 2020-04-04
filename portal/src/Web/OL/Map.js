"use strict";

// Get hold of the OpenLayer types and functions
var ol    = require ('ol');
var Map   = ol.Map;
var View  = ol.View;
var TileLayer = require ('ol/layer/Tile').default;
var OSM       = require ('ol/source/OSM').default;
var fromLonLat = require ('ol/proj').fromLonLat;

// Create a map to test with
exports.createMap = function (element) {
  return function (lon) {
    return function (lat) {
      return function (z) {
        return function() {
          return new Map({
            target: element,
            layers: [
              new TileLayer({
                source: new OSM()
              })
            ],
            view: new View({
              center: fromLonLat([lon, lat]),
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
        v.setCenter (fromLonLat([lon, lat]));
      }
    }
  }
}

// Removes the target from the map
exports.removeTarget = function (map) {
  return function () {
      map.setTarget (undefined);
  }
}
