;(function($) {
    $.fn.counter = function(options) {
        // Set default values
        var defaults = {
            start: 0,
            end: 10,
            time: 10,
            step: 1000,
            callback: function() { }
        }
        var options = $.extend(defaults, options);
        var _this = $(this);
        // The actual function that does the counting
        function counterFunc(el, increment, end){
            var value = parseInt(el.html(), 10) + increment;
            if(value >= end) {
                el.html(Math.round(end));
                options.callback();
            } else {
                el.html(Math.round(value));
                setTimeout(function () {
                    counterFunc(el, increment, end)
                },options.step);
            }
        }

        $(this).html(Math.round(options.start));

        var increment = (options.end - options.start) / ((1000 / options.step) * options.time);

        setTimeout(function () {
            counterFunc(_this, increment, options.end)
        },options.step);
    }
})(jQuery);