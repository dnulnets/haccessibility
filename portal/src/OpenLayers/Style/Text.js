//
// The Openlayers Interaction Text API mapping for purescript.
//
// This is just a very crude mapping and only helps out with what I need for this application. It is no
// complete mapping.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

// Get hold of the OpenLayer types and functions
var ol  = require ('ol');
var olst  = require ('ol/style');

exports.createImpl = function (opt) {
    return function() {
        console.log ('Text.create:', opt)
        var r = new olst.Text(opt);
        console.log ('Text.create.return', r);
        return r;
    }
}