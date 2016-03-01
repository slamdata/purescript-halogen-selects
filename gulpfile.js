'use strict';

var gulp = require("gulp"),
    purescript = require("gulp-purescript");

var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];

gulp.task("make", function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});
