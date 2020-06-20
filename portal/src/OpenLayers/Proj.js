//
// The Openlayers Projection API mapping for purescript.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');
var olp  = require ('ol/proj');

exports.epsg_3857 = "EPSG:3857"
exports.epsg_4326 = "EPSG:4326"

exports.fromLonLatImpl = function (coord,srs) {
    return olp.fromLonLat (coord, srs);
}

exports.toLonLatImpl = function (coord,srs) {
    return olp.fromLonLat (coord, srs);
}