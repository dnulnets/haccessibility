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
/*var ol  = require ('ol');
var oll = require ('ol/layer');
var olc = require ('ol/control')
var ols = require ('ol/source');
var olp = require ('ol/proj');
var olst = require ('ol/style');
var olg = require ('ol/geom');
var olgp = require ('ol/geom/Polygon');*/

var oli = require ('ol/interaction');

exports.createImpl = function (opts) {
    return function() {
        return new oli.Select (opts);
    }
}

exports.onImpl = function (event, f, self) {
    return function () {
        return self.on (event, function(e) {
            f(e)();
        });
    }    
}

exports.unImpl = function (event, key, self) {
    return function () {
        return self.un (event, key);
    }    
}
