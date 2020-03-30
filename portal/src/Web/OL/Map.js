"use strict";

// Get hold of the OpenLayer types and functions
var ol    = require ('ol');
var Map   = ol.Map;
var View  = ol.View;
var TileLayer = require ('ol/layer/Tile').default;
var OSM       = require ('ol/source/OSM').default;

// Create a map to test with
exports.createMap = function () {
      return new Map({
        target: 'map',
        layers: [
          new TileLayer({
            source: new OSM()
          })
        ],
        view: new View({
          center: [0, 0],
          zoom: 0
        })
      });
  };
