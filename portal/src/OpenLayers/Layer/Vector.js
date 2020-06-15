//
// The Openlayers Vector API mapping for purescript.
//
// This is just a very crude mapping and only helps out with what I need for this application. It is no
// complete mapping.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');
var oll  = require ('ol/layer');

exports.createImpl = function (opt) {
    return function() {
        return new oll.Vector(opt);
    }
}

exports.setStyleImpl = function (s, self) {
    return function() {
        self.setStyle(s);
    }
}

exports.setStyleFImpl = function (sf, self) {
    return function() {
        self.setStyle(function (feature, resolution) {
            return sf(feature)(resolution)();
        });
    }
}

exports.setStyleAImpl = exports.setStyleImpl

