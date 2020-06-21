//
// The Openlayers PluggableMap API mapping for purescript.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');

exports.addInteractionImpl = function (i, self) {
    return function() {
        self.addInteraction(i);
    }
}

exports.addLayerImpl = function (layer, self) {
    return function() {
        self.addLayer(layer);
    }
}

exports.getViewImpl = function (self) {
    return function() {
        return self.getView();
    }
}

exports.setTargetImpl = function (s, self) {
    return function() {
        return self.setTarget(s);
    }
}
