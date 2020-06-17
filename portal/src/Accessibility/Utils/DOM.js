//
// Som DOM manipulation functions that I need
//
// Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
//
"use strict";

exports.setInnerHTML = function (element) {
    return function (html) {
        return function () {
            element.innerHTML = html;
        }
    }
}

