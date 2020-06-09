//
// The Openlayers Interaction Select API mapping for purescript.
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

exports.getImpl = function (name, self) {
    return function() {
        return self.get(name);
    }
}
