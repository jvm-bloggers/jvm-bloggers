/**
 * @function      Include
 * @description   Includes an external scripts to the page
 * @param         {string} scriptUrl
 */
function include(scriptUrl) {
    document.write('<script src="' + scriptUrl + '"></script>');
}

// Quick and simple cross-browser event handler - to compensate for IE's attachEvent handler
function addEvent(obj, evt, fn, capture) {
    if (window.attachEvent) {
        obj.attachEvent("on" + evt, fn);
    } else {
        if (!capture) capture = false; // capture
        obj.addEventListener(evt, fn, capture)
    }
}

function Selector_Cache() {
    var collection = {};

    function get_from_cache(selector) {
        if (undefined === collection[selector]) {
            collection[selector] = $(selector);
        }

        return collection[selector];
    }

    return {
        get: get_from_cache
    };
}

var selectors = new Selector_Cache();

/**
 * @function      isIE
 * @description   checks if browser is an IE
 * @returns       {number} IE Version
 */
function isIE() {
    var myNav = navigator.userAgent.toLowerCase();
    return (myNav.indexOf('msie') != -1) ? parseInt(myNav.split('msie')[1]) : false;
};




(function($) {
    'use strict';

    var currentYear = (new Date).getFullYear();


    /**
     * @module       IE Fall&Polyfill
     * @description  Adds some loosing functionality to old IE browsers
     */

    if (isIE() && isIE() < 11) {
        include('assets/js/pointer-events.min.js');
        selectors.get('html').addClass('lt-ie11');

    }

    if (isIE() && isIE() < 10) {
        selectors.get('html').addClass('lt-ie10');
    }

    /**
     * @module       WOW Animation
     * @description  Enables scroll animation on the page
     */

    if (selectors.get('html').hasClass('desktop') && selectors.get('html').hasClass("wow-animation") && selectors.get(".wow").length) {
        include('assets/js/wow.min.js');
    }

    /**
     * @module       Smoothscroll
     * @description  Enables smooth scrolling on the page
     */

    if (selectors.get("html").hasClass("smoothscroll")) {
        include('assets/js/smoothscroll.min.js');
    }

    /**
     * @module       RD Smoothscroll
     * @description  Enables smooth scrolling on the page for all platforms
     */

    if (selectors.get("html").hasClass("smoothscroll-all")) {
        include('assets/js/rd-smoothscroll.min.js');
    }


    /**
     * @module       ToTop
     * @description  Enables ToTop Plugin
     */
    if (selectors.get('html').hasClass('desktop')) {
        include('assets/js/jquery.ui.totop.min.js');
    }


    /**
     * @module       Responsive Tabs
     * @description  Enables Easy Responsive Tabs Plugin
     */

    if (selectors.get('.responsive-tabs').length > 0) {
        include('assets/js/jquery.easy-responsive-tabs.min.js');
    }


    /**
     * @module       RD Google Map
     * @description  Enables RD Google Map Plugin
     */

    if (selectors.get('#google-map').length) {
        include('https://maps.google.com/maps/api/js');
        include('assets/js/jquery.rd-google-map.min.js');
    }


    /**
     * @module       RD Navbar
     * @description  Enables RD Navbar Plugin
     */
    if (selectors.get('.rd-navbar').length > 0) {
        include('assets/js/jquery.rd-navbar.min.js');
    }


    /**
     * @module       Progress Bar custom
     * @description  Enables Progress Bar Plugin
     */

    if (selectors.get(".progress-bar-custom").length) {
        include('assets/js/progressbar.min.js');
    }


    /**
     * @module       Count To
     * @description  Enables Count To Plugin
     */
    if (selectors.get('.counter').length > 0) {
        include('assets/js/jquery.countTo.js');
    }



    /**
     * @module      Progress Horizontal Bootstrap
     * @description  Enables Animation
     */
    if (selectors.get('.progress-bar').length > 0) {
        include('assets/js/jquery.counter.js');
    }


    /**
     * @module       tooltip
     * @description  Bootstrap tooltips
     */

    selectors.get('[data-toggle="tooltip"]').tooltip()


    /**
     * @module       Modal
     * @description  Bootstrap Modal
     */

    selectors.get('#myModal').modal('show')

    /**
     * @module       Dropdown
     * @description  Bootstrap Dropdown
     */

    selectors.get('.dropdown-toggle').dropdown()


    /**
     * @module       Tabs
     * @description  Bootstrap tabs
     */

    selectors.get('#myTabs a').on('click', function(e) {
        e.preventDefault()
        $(this).tab('show')
    })

    selectors.get('#myTabs2 a').on('click', function(e) {
        e.preventDefault()
        $(this).tab('show')
    })


    /**
     * @module     Owl Carousel
     * @description Enables Owl Carousel Plugin
     */

    if (selectors.get('.owl-carousel').length) {
        include('assets/js/jquery.owl-carousel.js');

        var isTouch = "ontouchstart" in window;

        function preventScroll(e) {
            e.preventDefault();
        }
    }



    /**
     * @module       Scroll To
     * @description  Enables Scroll To
     */

    if (selectors.get('.faq-section').length) {
        include('assets/js/scrollTo.js');
    }



    /**
     * @module       RD Search
     * @description  Enables RD Search Plugin
     */

    if (selectors.get('.rd-navbar-search').length) {

        include('assets/js/jquery.search.min.js');

    }



    /**
     * @module       TimeCircles
     * @description  Enables RD Search Plugin
     */
    if (selectors.get('#DateCountdown'.length)) {
        include('assets/js/TimeCircles.js');
    }




    /**
     * @module       Countdown
     * @description  Enables Countdown Plugin
     */


    if (selectors.get('.countdown').length) {
        include('assets/js/jquery.plugin.min.js');
        include('assets/js/jquery.countdown.js');

        var datecw = new Date();
        datecw.setTime(Date.parse(selectors.get('.countdown').attr('data-time'))).toLocaleString();
    }




    /**
     * @module       Magnific Popup
     * @description  Enables Magnific Popup Plugin
     */

    if (selectors.get('[data-lightbox]').not('[data-lightbox="gallery"] [data-lightbox]').length > 0 || selectors.get('[data-lightbox^="gallery"]').length > 0) {

        include('assets/js/jquery.magnific-popup.min.js');

    }



    /**
     * @module       Isotope
     * @description  Enables Isotope Plugin
     */

    if (selectors.get(".isotope").length) {
        include('assets/js/isotope.pkgd.min.js');
    }


    /**
     * @module       Onclick functions
     * @description  Add ... to onclick
     */

    if (selectors.get('.timeline').length) {

        function lineWidth() {
            var col = selectors.get('.timeline').find('.left-part').first();
            if (window.innerWidth > 991) {
                var colRightPosition = col.offset().left + col.outerWidth() - 15, //15px - padding
                    center = (selectors.get('.timeline').offset().left + selectors.get('.timeline').outerWidth()) / 2,
                    width = center - colRightPosition - 3; // 3px - offset from article
            } else {
                var colLeftPosition = col.find('article').offset().left,
                    width = colLeftPosition - 15 - 3; //15px - timeline left position in css, 3px - offset from article
                console.log(colLeftPosition);

            }
            selectors.get('.timeline').find('.line').css('width', width);
        }

    }



    /**
     * @module     RD Input Label
     * @description Enables RD Input Label Plugin
     */

    if (selectors.get('.form-label').length) {
        include('assets/js/mailform/jquery.rd-input-label.js');
    }


    /* Mailform
     =============================================*/

    if (selectors.get('.rd-mailform').length > 0) {
        include('assets/js/mailform/jquery.form.min.js');
        include('assets/js/mailform/jquery.rd-mailform.min.js');
    }

    /**
     * @module       FB
     * @description  FB comments
     */
    if (selectors.get('#fb-root').length) {
        (function(d, s, id) {
            var js, fjs = d.getElementsByTagName(s)[0];
            if (d.getElementById(id)) return;
            js = d.createElement(s);
            js.id = id;
            js.src = "http://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.5";
            fjs.parentNode.insertBefore(js, fjs);
        }(document, 'script', 'facebook-jssdk'));
    }



    /**
     * @module       Flickr
     * @description  Flickr
     */

    if (selectors.get('.flickr').length) {
        include('assets/js/rd-flickrfeed.min.js');
    }



    /**
     * @module       RD Parallax 3.5.0
     * @description  Enables RD Parallax 3.5.0 Plugin
     */

    if (selectors.get('.rd-parallax').length) {
        include('assets/js/jquery.rd-parallax.min.js');
    }


    /**
     * @module       Swiper Slider
     * @description  Enables Swiper Plugin
     */

    if (selectors.get(".swiper-slider").length) {
        include('assets/js/jquery.swiper.min.js');



    }


    $(document).ready(function() {

        /**
         * @module       Copyright
         * @description  Evaluates the copyright year
         */

        selectors.get("#copyright-year").text((new Date).getFullYear());

        // @module       IE Fall&Polyfill	
        if (isIE() && isIE() < 11) {
            PointerEventsPolyfill.initialize({});
        }

        // WOW Animation
        if (selectors.get('html').hasClass('desktop') && selectors.get('html').hasClass("wow-animation") && selectors.get(".wow").length) {
            new WOW().init();
        }

        // To Top
        if (selectors.get('html').hasClass('desktop')) {
            $().UItoTop({
                easingType: 'easeOutQuart',
                containerClass: 'ui-to-top fa fa-angle-up'
            });
        }

        // Responsive Tabs
        if (selectors.get('.responsive-tabs').length > 0) {
            selectors.get('.responsive-tabs').each(function() {
                var $this = $(this);
                $this.easyResponsiveTabs({
                    type: $this.attr("data-type") === "accordion" ? "accordion" : "default"
                });
            })
        }

        // Google Map
        if (selectors.get('#google-map').length) {
            var head = document.getElementsByTagName('head')[0],
                insertBefore = head.insertBefore;

            head.insertBefore = function(newElement, referenceElement) {
                if (newElement.href && newElement.href.indexOf('https://fonts.googleapis.com/css?family=Roboto') != -1 || newElement.innerHTML.indexOf('gm-style') != -1) {
                    return;
                }
                insertBefore.call(head, newElement, referenceElement);
            };

            //lazyInit(selectors.get('#google-map'), function () {
            selectors.get('#google-map').googleMap({
                styles: [{
                    "featureType": "water",
                    "elementType": "geometry",
                    "stylers": [{
                        "color": "#e9e9e9"
                    }, {
                        "lightness": 17
                    }]
                }, {
                    "featureType": "landscape",
                    "elementType": "geometry",
                    "stylers": [{
                        "color": "#f5f5f5"
                    }, {
                        "lightness": 20
                    }]
                }, {
                    "featureType": "road.highway",
                    "elementType": "geometry.fill",
                    "stylers": [{
                        "color": "#ffffff"
                    }, {
                        "lightness": 17
                    }]
                }, {
                    "featureType": "road.highway",
                    "elementType": "geometry.stroke",
                    "stylers": [{
                        "color": "#ffffff"
                    }, {
                        "lightness": 29
                    }, {
                        "weight": 0.2
                    }]
                }, {
                    "featureType": "road.arterial",
                    "elementType": "geometry",
                    "stylers": [{
                        "color": "#ffffff"
                    }, {
                        "lightness": 18
                    }]
                }, {
                    "featureType": "road.local",
                    "elementType": "geometry",
                    "stylers": [{
                        "color": "#ffffff"
                    }, {
                        "lightness": 16
                    }]
                }, {
                    "featureType": "poi",
                    "elementType": "geometry",
                    "stylers": [{
                        "color": "#f5f5f5"
                    }, {
                        "lightness": 21
                    }]
                }, {
                    "featureType": "poi.park",
                    "elementType": "geometry",
                    "stylers": [{
                        "color": "#dedede"
                    }, {
                        "lightness": 21
                    }]
                }, {
                    "elementType": "labels.text.stroke",
                    "stylers": [{
                        "visibility": "on"
                    }, {
                        "color": "#ffffff"
                    }, {
                        "lightness": 16
                    }]
                }, {
                    "elementType": "labels.text.fill",
                    "stylers": [{
                        "saturation": 36
                    }, {
                        "color": "#333333"
                    }, {
                        "lightness": 40
                    }]
                }, {
                    "elementType": "labels.icon",
                    "stylers": [{
                        "visibility": "off"
                    }]
                }, {
                    "featureType": "transit",
                    "elementType": "geometry",
                    "stylers": [{
                        "color": "#f2f2f2"
                    }, {
                        "lightness": 19
                    }]
                }, {
                    "featureType": "administrative",
                    "elementType": "geometry.fill",
                    "stylers": [{
                        "color": "#fefefe"
                    }, {
                        "lightness": 20
                    }]
                }, {
                    "featureType": "administrative",
                    "elementType": "geometry.stroke",
                    "stylers": [{
                        "color": "#fefefe"
                    }, {
                        "lightness": 17
                    }, {
                        "weight": 1.2
                    }]
                }]
            });
        }


        // RD Nav Plugin
        if (selectors.get('.rd-navbar').length > 0) {

            var responsive = {};

            var aliaces = ["-xs-", "-sm-", "-md-", "-lg-"],
                values = [480, 768, 992, 1200],
                i, j, val;

            responsive[0] = {
                layout: selectors.get('.rd-navbar').attr("data-layout") || "rd-navbar-fixed",
                focusOnHover: selectors.get('.rd-navbar').attr("data-hover-on") === "true",
                stickUp: selectors.get('.rd-navbar').attr("data-stick-up") === "true"
            };

            for (i = 0; i < values.length; i++) {

                //for (j = i; j >= -1; j--) {
                val = '';
                if (selectors.get('.rd-navbar').attr("data" + aliaces[i] + "layout")) {
                    if (!responsive[values[i]]) responsive[values[i]] = {};
                    if (!responsive[values[i]]["layout"]) {
                        responsive[values[i]]["layout"] = selectors.get('.rd-navbar').attr("data" + aliaces[i] + "layout");
                    }
                }

                if (selectors.get('.rd-navbar').attr("data" + aliaces[i] + "hover-on")) {
                    if (!responsive[values[i]]) responsive[values[i]] = {};
                    if (!responsive[values[i]]["focusOnHover"]) {
                        val = selectors.get('.rd-navbar').attr("data" + aliaces[i] + "hover-on") === 'true';
                        responsive[values[i]]["focusOnHover"] = val;
                    }
                }

                if (selectors.get('.rd-navbar').attr("data" + aliaces[i] + "stick-up")) {
                    if (!responsive[values[i]]) responsive[values[i]] = {};
                    if (!responsive[values[i]]["stickUp"] && responsive[values[i]]["stickUp"] !== 0) {
                        val = selectors.get('.rd-navbar').attr("data" + aliaces[i] + "stickUp") === 'true';
                        responsive[values[i]]["stickUp"] = val;
                    }
                }
                //}
            }

            selectors.get('.rd-navbar').RDNavbar({
                responsive: responsive
            });

        }

        // Progress Bar
        if (selectors.get(".progress-bar-custom").length) {
			
			function isScrolledIntoView(elem) {
            var $window = $(window);
            return elem.offset().top + elem.outerHeight() >= $window.scrollTop() && elem.offset().top <= $window.scrollTop() + $window.height();
			}
			
            selectors.get(".progress-bar-custom").each(function() {
                var bar, type;
                if (
                    this.className.indexOf("progress-bar-horizontal") > -1
                ) {
                    type = 'Line';
                }

                if (
                    this.className.indexOf("progress-bar-radial") > -1
                ) {
                    type = 'Circle';
                }

                if (this.getAttribute("data-stroke") && this.getAttribute("data-value") && type) {
                    //console.log(this.offsetWidth);
                    //console.log(parseFloat(this.getAttribute("data-stroke")) / this.offsetWidth * 100);
                    bar = new ProgressBar[type](this, {
                        strokeWidth: Math.round(parseFloat(this.getAttribute("data-stroke")) / this.offsetWidth * 100),
                        trailWidth: this.getAttribute("data-trail") ? Math.round(parseFloat(this.getAttribute("data-trail")) / this.offsetWidth * 100) : 0,
                        text: {
                            value: this.getAttribute("data-counter") === "true" ? '0' : null,
                            className: 'progress-bar__body',
                            style: null
                        }
                    });

                    bar.svg.setAttribute('preserveAspectRatio', "none meet");
                    if (type === 'Line') {
                        bar.svg.setAttributeNS(null, "height", this.getAttribute("data-stroke"));
                    }

                    bar.path.removeAttribute("stroke");
                    bar.path.className.baseVal = "progress-bar__stroke";
                    if (bar.trail) {
                        bar.trail.removeAttribute("stroke");
                        bar.trail.className.baseVal = "progress-bar__trail";
                    }

                    if (this.getAttribute("data-easing") && !isIE()) {
                        $(document)
                            .on("scroll", $.proxy(function() {
                                //console.log(isScrolledIntoView(this));
                                if (isScrolledIntoView($(this)) && this.className.indexOf("progress-bar--animated") === -1) {
                                    console.log(1);
                                    this.className += " progress-bar--animated";
                                    bar.animate(parseInt(this.getAttribute("data-value")) / 100.0, {
                                        easing: this.getAttribute("data-easing"),
                                        duration: this.getAttribute("data-duration") ? parseInt(this.getAttribute("data-duration")) : 800,
                                        step: function(state, b) {
                                            if (b._container.className.indexOf("progress-bar-horizontal") > -1 ||
                                                b._container.className.indexOf("progress-bar-vertical") > -1) {
                                                b.text.style.width = Math.abs(b.value() * 100).toFixed(0) + "%"
                                            }
                                            b.setText(Math.abs(b.value() * 100).toFixed(0));
                                        }
                                    });
                                }
                            }, this))
                            .trigger("scroll");
                    } else {
                        bar.set(parseInt(this.getAttribute("data-value")) / 100.0);
                        bar.setText(this.getAttribute("data-value"));
                        if (type === 'Line') {
                            bar.text.style.width = parseInt(this.getAttribute("data-value")) + "%";
                        }
                    }
                } else {
                    console.error(this.className + ": progress bar type is not defined");
                }
            });

        }


        // Count To

        if (selectors.get('.counter').length > 0) {

            $(document).on("scroll", $.proxy(function() {
                    selectors.get('.counter').not('.animated').each(function() {
                        var $this = $(this);
                        var position = $this.offset().top;

                        if (($(window).scrollTop() + $(window).height()) > position) {

                            $this.countTo();
                            $this.addClass('animated');
                        }
                    });
                }, $(this)))
                .trigger("scroll");

        }


        /**
         * @module      Shop Gallery
         * @description  Custom
         */

        if (selectors.get('.shop-gallery').length > 0) {


            $(this).find('#' + $(this).attr('id')).removeClass('active');

            var gallPrevie = selectors.get("[data-shop-gallery]");

            gallPrevie.on("click", function() {


                var $t = $(this).attr('data-shop-gallery');
                console.log($(this));

                selectors.get("[data-lightbox]").removeClass("active");

                selectors.get('#' + $t).addClass('active');
            });


        }



        /**
         * @module      Progress Horizontal Bootstrap
         * @description  Enables Animation
         */
        if (selectors.get('.progress-bar').length > 0) {

            $(document).on("scroll", $.proxy(function() {
                    selectors.get('.progress-bar').not('.animated').each(function() {

                        var position = $(this).offset().top;

                        if (($(window).scrollTop() + $(window).height()) > position) {
                            var $this = $(this);
                            var start = $this.attr("aria-valuemin");
                            var end = $this.attr("aria-valuenow");

                            if ($this.hasClass("vertical-bar")) {
                                $this.css({
                                    height: end + '%'
                                });
                                $this.parent().find('span').css({
                                    bottom: end + '%'
                                });
                            } else {
                                $this.css({
                                    width: end + '%'
                                });
                                $this.parent().find('span').css({
                                    left: end + '%'
                                });
                            }


                            $this.parent().find('span').counter({
                                start: start,
                                end: end,
                                time: 0.4,
                                step: 20
                            });

                            //var span = $this.parent().find('span');
                            //
                            //span.prop('Counter', start).animate({
                            //    Counter: end
                            //}, {
                            //    duration: 1000,
                            //    easing: 'linear',
                            //    step: function (now) {
                            //        $(this).text(Math.ceil(now));
                            //    }
                            //});
                            $this.addClass('animated');
                        }

                    });
                }, $(this)))
                .trigger("scroll");

        }



        /**
         * @module     Owl Carousel
         * @description Enables Owl Carousel Plugin
         */

        if (selectors.get('.owl-carousel').length) {

            selectors.get('.owl-carousel').each(function() {
                var c = $(this),
                    responsive = {};

                var aliaces = ["-", "-xs-", "-sm-", "-md-", "-lg-"],
                    values = [0, 480, 768, 992, 1200],
                    i, j;

                for (i = 0; i < values.length; i++) {
                    responsive[values[i]] = {};
                    for (j = i; j >= -1; j--) {
                        if (!responsive[values[i]]["items"] && c.attr("data" + aliaces[j] + "items")) {
                            responsive[values[i]]["items"] = j < 0 ? 1 : parseInt(c.attr("data" + aliaces[j] + "items"));
                        }
                        if (!responsive[values[i]]["stagePadding"] && responsive[values[i]]["stagePadding"] !== 0 && c.attr("data" + aliaces[j] + "stage-padding")) {
                            responsive[values[i]]["stagePadding"] = j < 0 ? 0 : parseInt(c.attr("data" + aliaces[j] + "stage-padding"));
                        }
                        if (!responsive[values[i]]["margin"] && responsive[values[i]]["margin"] !== 0 && c.attr("data" + aliaces[j] + "margin")) {
                            responsive[values[i]]["margin"] = j < 0 ? 30 : parseInt(c.attr("data" + aliaces[j] + "margin"));
                        }
                    }
                }
                //console.log('string', c);
                c.owlCarousel({
                    autoplay: c.attr("data-autoplay") === "true",
                    loop: c.attr("data-loop") !== "false",
                    item: 1,
                    animateOut: c.attr("data-fadeout"),
                    mouseDrag: c.attr("data-mouse-drag") !== "false",
                    nav: c.attr("data-nav") === "true",
                    dots: c.attr("data-dots") === "true",
                    //dotsEach: c.attr("data-dots-each") ? parseInt(c.attr("data-dots-each")) : false,
                    dotsEach: true,
                    responsive: responsive,
                    navText: [],
                    onInitialized: function() {
                        if ($.fn.magnificPopup) {
                            var owlcarousel1 = this.$element.attr('data-lightbox') !== "gallery",
                                g = this.$element.attr('data-lightbox') === "gallery";

                            if (owlcarousel1) {
                                this.$element.each(function() {
                                    var $this = $(this);
                                    $this.magnificPopup({
                                        type: $this.attr("data-lightbox"),
                                        callbacks: {
                                            open: function() {
                                                if (isTouch) {
                                                    $(document).on("touchmove", preventScroll);
                                                    $(document).swipe({
                                                        swipeDown: function() {
                                                            $.magnificPopup.close();
                                                        }
                                                    });
                                                }
                                            },
                                            close: function() {
                                                if (isTouch) {
                                                    $(document).off("touchmove", preventScroll);
                                                    $(document).swipe("destroy");
                                                }
                                            }
                                        }
                                    });
                                })
                            }

                            if (g) {
                                this.$element.each(function() {
                                    var $gallery = $(this);

                                    $gallery
                                        .find('[data-lightbox]').each(function() {
                                            var $item = $(this);
                                            $item.addClass("mfp-" + $item.attr("data-lightbox"));
                                        })
                                        .end()
                                        .magnificPopup({
                                            delegate: '.owl-item:not(.cloned) .owl-item [data-lightbox]',
                                            type: "image",
                                            gallery: {
                                                enabled: true
                                            },
                                            callbacks: {
                                                open: function() {
                                                    if (isTouch) {
                                                        $(document).on("touchmove", preventScroll);
                                                        $(document).swipe({
                                                            swipeDown: function() {
                                                                $.magnificPopup.close();
                                                            }
                                                        });
                                                    }
                                                },
                                                close: function() {
                                                    if (isTouch) {
                                                        $(document).off("touchmove", preventScroll);
                                                        $(document).swipe("destroy");
                                                    }
                                                }
                                            }
                                        });
                                })
                            }
                        }
                    }
                });
            });

        }



        /**
         * @module       SVG-Animate
         * @description  Enables SVG-Animate *
         */

        if ((selectors.get('#svg-phone_1').length) && (!(!!navigator.userAgent.match(/Trident\/7\./)))) {



            $(this).on("scroll", $.proxy(function() {
                    selectors.get('#svg-phone_1').not('.active').each(function() {
                        var $this = $(this);
                        var position = $this.offset().top;

                        if (($(window).scrollTop() + $(window).height()) > position) {
                            $this.attr("class", "active");
                            $this.parent().find('.phone_1').addClass('active');
                        }
                    });
                }, $(this)))
                .trigger("scroll");

        }


        /**
         * @module       ViewPort Universal
         * @description  Add class in viewport
         */
        if (selectors.get('.view-animate').length) {



            $(this).on("scroll", $.proxy(function() {
                    selectors.get('.view-animate').not('.active').each(function() {
                        var $this = $(this);
                        var position = $this.offset().top;

                        if (($(window).scrollTop() + $(window).height()) > position) {
                            $this.addClass("active");
                        }
                    });
                }, $(this)))
                .trigger("scroll");

        }



        /**
         * @module       Scroll To
         * @description  Enables Scroll To
         */

        if (selectors.get('.faq-section').length) {

            selectors.get('.faq-section').scrollTo({});

        }



        /**
         * @module       RD Search
         * @description  Enables RD Search Plugin
         */
        if (selectors.get('.rd-navbar-search').length) {

            selectors.get('.rd-navbar-search').RDSearch({});

        }




        /**
         * @module       TimeCircles
         * @description  Enables RD Search Plugin
         */
        if (selectors.get('#DateCountdown'.length)) {

            var time = {
                "Days": {
                    "text": "Days",
                    "color": "#FFF",
                    "show": true
                },
                "Hours": {
                    "text": "Hours",
                    "color": "#fff",
                    "show": true
                },
                "Minutes": {
                    "text": "Minutes",
                    "color": "#fff",
                    "show": true
                },
                "Seconds": {
                    "text": "Seconds",
                    "color": "#fff",
                    "show": true
                }
            };
            selectors.get('#DateCountdown').TimeCircles({
                "animation": "smooth",
                "bg_width": 0.4,
                "fg_width": 0.02666666666666667,
                "circle_bg_color": "rgba(0,0,0,.2)",
                "time": time
            });
            $(window).on('load resize orientationchange', function() {
                if ($(window).width() < 479) {
                    selectors.get('#DateCountdown').TimeCircles({
                        time: {
                            //Days: {show: true},
                            //Hours: {show: true},
                            Minutes: {
                                show: true
                            },
                            Seconds: {
                                show: false
                            }
                        }
                    }).rebuild();
                } else if ($(window).width() < 767) {
                    selectors.get('#DateCountdown').TimeCircles({
                        time: {
                            //Minutes: {show: true},
                            Seconds: {
                                show: false
                            }
                        }
                    }).rebuild();
                } else {
                    selectors.get('#DateCountdown').TimeCircles({
                        time: time
                    }).rebuild();
                }
            });

        }



        /**
         * @module       Countdown
         * @description  Enables Countdown Plugin
         */

        if (selectors.get('.countdown').length) {

            var settings = [];

            settings[selectors.get('.countdown').attr('data-type')] = datecw;
            settings['format'] = selectors.get('.countdown').attr('data-format');

            selectors.get('.countdown').countdown(settings);
        }




        /**
         * @module       Magnific Popup
         * @description  Enables Magnific Popup Plugin
         */

        if (selectors.get('[data-lightbox]').not('[data-lightbox="gallery"] [data-lightbox]').length > 0 || selectors.get('[data-lightbox^="gallery"]').length > 0) {
            if (selectors.get('[data-lightbox]').not('[data-lightbox="gallery"] [data-lightbox]').length) {
                selectors.get('[data-lightbox]').not('[data-lightbox="gallery"] [data-lightbox]').each(function() {
                    var $this = $(this);
                    $this.magnificPopup({
                        type: $this.attr("data-lightbox")
                    });
                })
            }

            if (selectors.get('[data-lightbox^="gallery"]').length) {
                selectors.get('[data-lightbox^="gallery"]').each(function() {
                    var $gallery = $(this);
                    $gallery
                        .find('[data-lightbox]').each(function() {
                            var $item = $(this);
                            $item.addClass("mfp-" + $item.attr("data-lightbox"));
                        })
                        .end()
                        .magnificPopup({
                            delegate: '[data-lightbox]',
                            type: "image",
                            // Delay in milliseconds before popup is removed
                            removalDelay: 300,
                            // Class that is added to popup wrapper and background
                            // make it unique to apply your CSS animations just to this exact popup
                            mainClass: 'mfp-fade',
                            gallery: {
                                enabled: true
                            }
                        });
                })
            }

        }


        /**
         * @module       Isotope
         * @description  Enables Isotope Plugin
         */

        if (selectors.get(".isotope").length) {

            selectors.get(".isotope").each(function() {
                var _this = this,
                    iso = new Isotope(_this, {
                        itemSelector: '[class*="col-"], .isotope-item',
                        layoutMode: _this.getAttribute('data-layout') ? _this.getAttribute('data-layout') : 'masonry'
                    });

                $(window).on("resize", function() {
                    iso.layout();
                });

                $(window).load(function() {
                    iso.layout();
                    setTimeout(function() {
                        _this.className += " isotope--loaded";
                        iso.layout();
                    }, 600);
                });
            });

            selectors.get(".isotope-filters-trigger").on("click", function() {
                $(this).parents(".isotope-filters").toggleClass("active");
            });

            selectors.get('.isotope').magnificPopup({
                delegate: ' > :visible .mfp-image',
                type: "image",
                gallery: {
                    enabled: true
                },
            });

            selectors.get("[data-isotope-filter]").on("click", function() {
                selectors.get('[data-isotope-filter][data-isotope-group="' + this.getAttribute("data-isotope-group") + '"]').removeClass("active");
                $(this).addClass("active");
                $(this).parents(".isotope-filters").removeClass("active");
                selectors.get('.isotope[data-isotope-group="' + this.getAttribute("data-isotope-group") + '"]')
                    .isotope({
                        filter: this.getAttribute("data-isotope-filter") == '*' ? '*' : '[data-filter="' + this.getAttribute("data-isotope-filter") + '"]'
                    });
            })

        }



        /**
         * @module       Onclick functions
         * @description  Add ... to onclick
         */

        if (selectors.get('.timeline').length) {

            selectors.get('.timeline').find(".timeline-btn").on("click", function() {
                $(this).toggleClass("active");
                // selectors.get('.timeline').find(".timeline-hidden").toggleClass("active");
                if (selectors.get('.timeline').find(".timeline-hidden").is(':hidden')) {
                    selectors.get('.timeline').find(".timeline-hidden").slideDown(800);
                } else {
                    selectors.get('.timeline').find(".timeline-hidden").slideUp(800);
                }
            });

        }



        /**
         * @module     RD Input Label
         * @description Enables RD Input Label Plugin
         */

        if (selectors.get('.form-label').length) {

            selectors.get('.form-label').RDInputLabel();

        }


        /* Mailform
         =============================================*/


        if (selectors.get('.rd-mailform').length > 0) {

            var mailform = selectors.get('.rd-mailform');

            if (mailform.length) {
                mailform.rdMailForm({
                    validator: {
                        'constraints': {
                            '@LettersOnly': {
                                message: 'Please use letters only!'
                            },
                            '@NumbersOnly': {
                                message: 'Please use numbers only!'
                            },
                            '@NotEmpty': {
                                message: 'Field should not be empty!'
                            },
                            '@Email': {
                                message: 'Enter valid e-mail address!'
                            },
                            '@Phone': {
                                message: 'Enter valid phone number!'
                            },
                            '@Date': {
                                message: 'Use MM/DD/YYYY format!'
                            },
                            '@SelectRequired': {
                                message: 'Please choose an option!'
                            }
                        }
                    }
                }, {
                    'MF000': 'Sent',
                    'MF001': 'Recipients are not set!',
                    'MF002': 'Form will not work locally!',
                    'MF003': 'Please, define email field in your form!',
                    'MF004': 'Please, define type of your form!',
                    'MF254': 'Something went wrong with PHPMailer!',
                    'MF255': 'There was an error submitting the form!'
                });
            }
        }


        /**
         * @module       Flickr
         * @description  Flickr
         */
        if (selectors.get('.flickr').length) {
            selectors.get('.flickr').RDFlickr({});
        }


        /**
         * @module       RD Parallax 3.5.0
         * @description  Enables RD Parallax 3.5.0 Plugin
         */

        if (selectors.get('.rd-parallax').length) {

            selectors.get('.rd-parallax').each(function() {
                if (!$(this).parents(".swiper-slider").length) {
                    $.RDParallax();
                }
            });

        }


        /**
         * @module       Swiper Slider
         * @description  Enables Swiper Plugin
         */

        if (selectors.get(".swiper-slider").length) {

            function getSwiperHeight(object, attr) {
                var val = object.attr("data-" + attr),
                    dim;

                if (!val) {
                    return undefined;
                }

                dim = val.match(/(px)|(%)|(vh)$/i);

                if (dim.length) {
                    switch (dim[0]) {
                        case "px":
                            return parseFloat(val);
                        case "vh":
                            return $(window).height() * (parseFloat(val) / 100);
                        case "%":
                            return object.width() * (parseFloat(val) / 100);
                    }
                } else {
                    return undefined;
                }
            }

            function toggleSwiperInnerVideos(swiper) {
                var prevSlide = $(swiper.slides[swiper.previousIndex]),
                    nextSlide = $(swiper.slides[swiper.activeIndex]),
                    videos;

                prevSlide.find("video").each(function() {
                    this.pause();
                });

                videos = nextSlide.find("video");
                if (videos.length) {
                    videos.get(0).play();
                }
            }

            function toggleSwiperCaptionAnimation(swiper) {
                var prevSlide = $(swiper.container),
                    nextSlide = $(swiper.slides[swiper.activeIndex]);

                prevSlide
                    .find("[data-caption-animate]")
                    .each(function() {
                        var $this = $(this);
                        $this
                            .removeClass("animated")
                            .removeClass($this.attr("data-caption-animate"))
                            .addClass("not-animated");
                    });

                nextSlide
                    .find("[data-caption-animate]")
                    .each(function() {
                        var $this = $(this),
                            delay = $this.attr("data-caption-delay");

                        setTimeout(function() {
                            $this
                                .removeClass("not-animated")
                                .addClass($this.attr("data-caption-animate"))
                                .addClass("animated");
                        }, delay ? parseInt(delay) : 0);
                    });
            }


            selectors.get(".swiper-slider").each(function() {
                var s = $(this);

                var pag = s.find(".swiper-pagination"),
                    next = s.find(".swiper-button-next"),
                    prev = s.find(".swiper-button-prev"),
                    bar = s.find(".swiper-scrollbar"),
                    h = getSwiperHeight(selectors.get(".swiper-slider"), "height"),
                    mh = getSwiperHeight(selectors.get(".swiper-slider"), "min-height");
                s.find(".swiper-slide")
                    .each(function() {
                        var $this = $(this),
                            url;
                        if (url = $this.attr("data-slide-bg")) {
                            $this.css({
                                "background-image": "url(" + url + ")",
                                "background-size": "cover"
                            })
                        }
                    })
                    .end()
                    .find("[data-caption-animate]")
                    .addClass("not-animated")
                    .end()
                    .swiper({
                        autoplay: s.attr('data-autoplay') ? s.attr('data-autoplay') === "false" ? undefined : s.attr('data-autoplay') : 5000,
                        direction: s.attr('data-direction') ? s.attr('data-direction') : "horizontal",
                        effect: s.attr('data-slide-effect') ? s.attr('data-slide-effect') : "slide",
                        speed: s.attr('data-slide-speed') ? s.attr('data-slide-speed') : 600,
                        keyboardControl: s.attr('data-keyboard') === "true",
                        mousewheelControl: s.attr('data-mousewheel') === "true",
                        mousewheelReleaseOnEdges: s.attr('data-mousewheel-release') === "true",
                        nextButton: next.length ? next.get(0) : null,
                        prevButton: prev.length ? prev.get(0) : null,
                        pagination: pag.length ? pag.get(0) : null,
                        allowSwipeToNext: true,
                        allowSwipeToPrev: true,
                        paginationClickable: pag.length ? pag.attr("data-clickable") !== "false" : false,
                        paginationBulletRender: pag.length ? pag.attr("data-index-bullet") === "true" ? function(index, className) {
                            return '<span class="' + className + '">' + (index + 1) + '</span>';
                        } : null : null,
                        scrollbar: bar.length ? bar.get(0) : null,
                        scrollbarDraggable: bar.length ? bar.attr("data-draggable") !== "false" : true,
                        scrollbarHide: bar.length ? bar.attr("data-draggable") === "false" : false,
                        loop: s.attr('data-loop') !== "false",
                        simulateTouch: false,
                        //threshold: 2000,
                        onTransitionStart: function(swiper) {
                            toggleSwiperInnerVideos(swiper);
                        },
                        onTransitionEnd: function(swiper) {
                            toggleSwiperCaptionAnimation(swiper);
                        },
                        onInit: function(swiper) {
                            toggleSwiperInnerVideos(swiper);
                            toggleSwiperCaptionAnimation(swiper);
                        }
                    });

                $(window)
                    .on("resize", function() {
                        var mh = getSwiperHeight(s, "min-height"),
                            h = getSwiperHeight(s, "height");
                        if (h) {
                            s.css("height", mh ? mh > h ? mh : h : h);
                        }
                    })
                    .trigger("resize");
            });
        }

        // Gallery init

        if (selectors.get('.swiper-container').length) {


            var galleryTop = new Swiper('.gallery-top', {
                nextButton: '.swiper-button-next',
                prevButton: '.swiper-button-prev',
                spaceBetween: 10
            });

            var galleryThumbs = new Swiper('.gallery-thumbs', {
                spaceBetween: 10,
                centeredSlides: true,
                slidesPerView: 'auto',
                touchRatio: 0.2,
                slideToClickedSlide: true
            });

            galleryTop.params.control = galleryThumbs;
            galleryThumbs.params.control = galleryTop;




            // Get some unordered list, which contains anchor tags
            var galleryid = document.getElementById('gall');



            // Check to see if the node that was clicked is an anchor tag. If so, proceed per usual. 
            addEvent(galleryid, "click", function(e) {
                // Firefox and IE access the target element different. e.target, and event.srcElement, respectively.
                var target = e ? e.target : window.event.srcElement;
                if (target.nodeName.toLowerCase() === 'a') {
                    galleryThumbs.slideTo(galleryid, 1000, false);
                    return false;
                }
            });


        }


    });

})($);