//
// The Openlayers Map API mapping for purescript.
//
// This is just a very crude mapping and only helps out with what I need for this application. It is no
// complete mapping.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');

exports.createImpl = function (opt) {
    return function() {
        var r = new ol.Map(opt);
        return r;
    }
}

exports.addLayerImpl = function (layer, self) {
    return function() {
        console.log ('Map.addLayer:', layer, self)
        self.addLayer(layer);
    }
}

exports.getViewImpl = function (self) {
    return function() {
        return self.getView();
    }
}