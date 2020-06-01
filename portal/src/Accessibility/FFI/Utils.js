//
// The FFI Utility mapping for purescript.
//
// This is a crude mapping and helps out with some stuff I have not figured out how to do
// in purescript.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

//
// Enable tooltips for all data-toggle="tooltip" in Bootstrap4
//
exports.enableTooltipsImpl = function() {
    return function () {
      $('[data-toggle="tooltip"]').tooltip()
    }
  }
