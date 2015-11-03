"use strict";

// module LocalStorage

exports.getImpl = function (key) {
  return function () {
    return window.localStorage.getItem(key);
  };
};

exports.setImpl = function (key) {
  return function (value) {
    return function () {
      window.localStorage.setItem(key, value);
      return {};
    };
  };
};

