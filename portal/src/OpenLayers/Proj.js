//
// The Openlayers Proj API mapping for purescript.
//
// This is just a very crude mapping and only helps out with what I need for this application. It is no
// complete mapping.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var olp  = require ('ol/proj');

exports.fromLonLatImpl = function (coord,srs) {
    olp.fromLonLat (coord, srs);
}

exports.toLonLatImpl = function (coord,srs) {
    olp.fromLonLat (coord, srs);
}