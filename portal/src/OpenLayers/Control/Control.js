//
// The Openlayers Control API mapping for purescript.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var olc  = require ('ol/control');

exports.createImpl = function (opt) {
    return function() {
        var r = new olc.Control(opt);
        return r;
    }
}
