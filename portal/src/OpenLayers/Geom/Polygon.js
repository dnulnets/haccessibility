//
// The Openlayers Polygon API mapping for purescript.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');
var olg = require ('ol/geom');

exports.createImpl = function (ap, la) {
    return function () {
        return new olg.Polygon (ap,la);
    }
}
