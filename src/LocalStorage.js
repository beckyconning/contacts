"use strict";

// module LocalStorage

exports.get = function (key) {
  return function () {
    return window.localStorage.getItem(key);
  };
};

exports.set = function (key) {
  return function (value) {
    return function () {
      window.localStorage.setItem(key, val);
      return {};
    };
  };
};
