;
(function ($) {
    $.fn.scrollTo = function () {
        //smoothscroll
        $(this).find('a[href^="#"]').on('click', function (e) {
            //e.preventDefault();
            $(this).addClass('active');

            var target = this.hash,
            $target = $(target);
            $('html, body').stop().animate({
                'scrollTop': $target.offset().top + 2
            }, 1000, 'swing', function () {
                window.location.hash = target;
            });
        });
    }
})(jQuery);