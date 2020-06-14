//
// The Openlayers FFI Purescript helper functions
//
// This file contains a set of functions to help prepare function calls for the
// javascript functins.
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

/*
** Example:
**
** Javascript
**
** exports.setCenterImpl = effize("setCenter");
**
** Purescript
**
** foreign import setCenterImpl :: Fn2(Array Number) View (Effect Unit)
**
** setCenter :: Array Number -> View -> Effect Unit
** setCenter pos self = runFn2 setCenterImpl pos self
**
*/
exports.effize = function (method) {
    return function () {
        var me = arguments[arguments.length - 1];
        var args = Array.prototype.slice.call(arguments, 0, -1);
        return function () {
            console.log ('EFFIZE:', method, me, args);
            return me[method].apply(me, args);
        };
    };
}

/*
** Example:
**
** Javascript
**
** exports.getName = effgetfield("name");
**
** Purescript
**
** foreign import getName :: View->Effect String
**
*/
exports.effgetfield = function(field) {
    return function (self) {
        return function () {
            console.log ('EFFGETFIELD:', field, self);
            return self[field];
        };
    };
}

/*
** Example:
**
** Javascript
**
** exports.setName = effsetfield("name");
**
** Purescript
**
** foreign import setName :: String->View->Effect Unit
**
*/
exports.effsetfield = function(field) {
    return function (value) {
        return function (self) {
            return function () {
                console.log ('EFFSETFIELD:', value, field, self);
                self[field]=value;
            };
        };
    };
}
