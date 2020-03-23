/* global exports */
"use strict";

exports.geolocation = function (navigator) {
    return function () {
        console.log ("Get the location device");
        return navigator.geolocation;
    }
};

exports._getCurrentPosition = function (options) {
    return function (geolocation) {
        return function (onError, onSuccess) {
            geolocation.getCurrentPosition(function (position) {
                onSuccess(position);
            }, function (positionError) {
                console.log ("Exception: " + positionError.message + "," + positionError.code)
                onError(positionError);
            }, options);
            return function (cancelError, onCancelerError, onCancelerSuccess) {
                onCancelerSuccess();
            };
        };
    };
};

exports._watchPosition = function(Tuple) {
    return function (options) {
        return function (watch) {
            return function (watchError) {
                return function (geolocation) {
                    return function (onError, onSuccess) {
                        var watchId = geolocation.watchPosition(function (position) {
                            watch (new Tuple(watchId, position));
                        }, function (positionError) {
                            console.log ("Exception: " + watchID + "," + positionError.message + "," + positionError.code)
                            watchError (new Tuple(watchId, positionError));
                        }, options);            
                        onSuccess(watchId);
                        return function (cancelError, onCancelerError, onCancelerSuccess) {
                            onCancelerSuccess();
                        };
                    };
                };
            };
        };
    };
};

exports._clearWatch = function (watchId) {
    return function (geolocation) {
        return function() {
            geolocation.clearWatch(watchId);
        };
    };
};
