//
// The Openlayers Geolocation API mapping for purescript.
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
        console.log ('Geolocation:', opt);
        return new ol.Geolocation(opt);
    }
}


exports.onImpl = function (event, f, self) {
    return function () {
        return self.on (event, function(e) {
            f(e)();
        });
    }    
}

exports.onceImpl = function (event, f, self) {
    return function () {
        return self.once (event, function(e) {
            f(e)();
        });
    }    
}

exports.unImpl = function (event, key, self) {
    return function () {
        return self.un (event, key);
    }    
}

exports.getAccuracyGeometryImpl = function (self) {
    return function () {
        return self.getAccuracyGeometry();
    }
}

exports.getPositionImpl = function (self) {
    return function () {
        return self.getPosition();
    }
}

exports.getAltitudeImpl = function (self) {
    return function () {
        return self.getAltitude();
    }
}

exports.getAltitudeAccuracyImpl = function (self) {
    return function () {
        return self.getAltitudeAccuracy();
    }
}

exports.getAccuracyImpl = function (self) {
    return function () {
        return self.getAccuracy();
    }
}

exports.setTrackingImpl = function (onoff, self) {
    return function () {
        self.setTracking (onoff);
    }
}