//
// The Openlayers Feature API mapping for purescript.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');

exports.createImpl = function (opt) {
    return function() {
        return new ol.Feature(opt);
    }
}

exports.setStyleImpl = function (r, self) {
    return function () {
        self.setStyle (r);
    }
}

exports.setPropertiesImpl = function (r, self) {
    return function () {
        self.setProperties (r);
    }
}

exports.setGeometryImpl = function (r, self) {
    return function () {
        self.setGeometry (r);
    }
}
