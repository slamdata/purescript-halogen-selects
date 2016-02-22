// module SlamData.Halogen.Select.Utils.Random

exports.toString = function(base) {
    return function(num) {
        return num.toString(base);
    };
};
