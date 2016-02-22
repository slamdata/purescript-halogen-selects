// module SlamData.Halogen.Select.Utils.DOM

exports.getScreen = function() {
    var ws = window.screen;
    return {
        width: ws.width,
        height: ws.height
    };
};

exports.getComputedStyle = function(el) {
    return function() {
        return getComputedStyle(el);
    };
};

exports.getClientRects = function(el) {
    return function() {
        var rects = el.getClientRects(),
            result = [];
        for (var i = 0; i < rects.length; i++) {
            result.push(rects[i]);
        }
        return result;
    };
};
