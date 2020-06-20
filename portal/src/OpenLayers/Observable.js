//
// The Openlayers Observable API mapping for purescript.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');

exports.onImpl = function (e, f, self) {
    return function() {
        return self.on (e, function(event) {
            return f(event)();
        });
    }
}

exports.onceImpl = function (e, f, self) {
    return function () {
        return self.once (e, function(event) {
            return f(event)();
        });
    }    
}

exports.unImpl = function (e, key, self) {
    return function () {
        return self.un (e, key);
    }    
}
