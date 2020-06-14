//
// The Openlayers View API mapping for purescript.
//
// This is just a very crude mapping and only helps out with what I need for this application. It is no
// complete mapping.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');

// Helper functions for purescripts FFI
var p = require ('./src/OpenLayers/ffi.js')

// new operator for a View
exports.createImpl = function (opt) {
    return function() {
        var r = new ol.View(opt);
        return r;
    }
}

// Setters
exports.setCenterImpl = p.effize("setCenter");

// Getters
exports.getProjectionImpl = p.effize("getProjection");
