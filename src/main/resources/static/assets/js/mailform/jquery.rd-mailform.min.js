/**
 * @module       RD Mail Form
 * @version      1.1.0
 * @author       Evgeniy Gusarov
 * @see          https://ua.linkedin.com/pub/evgeniy-gusarov/8a/a40/54a
 */
!function (e) {
    function t(o, s, n) {
        i = e.extend(!0, {}, i, n), this.options = e.extend(!0, {}, t.Defaults, s), this.$element = e(o), this._plugins = {}, this._handlers = {
            "mf.success mf.fail": e.proxy(this.update, this),
            "mf.process": e.proxy(this.process, this),
            reset: e.proxy(this.reset, this)
        }, e.each(t.Plugins, e.proxy(function (e, t) {
            this._plugins[e[0].toLowerCase() + e.slice(1)] = new t(this)
        }, this)), this.initialize()
    }

    var i;
    i = {
        MF000: "Sent",
        MF001: "Recipients are not set!",
        MF002: "Form will not work locally!",
        MF003: "Please, define email field in your form!",
        MF004: "Please, define type of your form!",
        MF254: "Something went wrong with PHPMailer!",
        MF255: "Aw, snap! Something went wrong."
    }, t.Defaults = {baseClass: "rd-mailform"}, t.Plugins = {}, t.prototype.initialize = function () {
        this.$element.trigger("mf.initialize"), this.$element.addClass(this.options.baseClass).trigger("reset"), this.create(), this.watch(), this.$element.trigger("mf.initialized")
    }, t.prototype.create = function () {
    }, t.prototype.watch = function () {
        var e = this;
        e.$element.ajaxForm({
            beforeSubmit: function () {
                e.$element.trigger("mf.process")
            }, error: function (t) {
                e.$element.trigger("mf.fail", {code: t, message: i[t]})
            }, success: function (t) {
                console.log(t), "MF000" == t ? e.$element.trigger("mf.success", {
                    code: t,
                    message: i[t]
                }) : (t = 5 == t.length ? t : "MF255", e.$element.trigger("mf.fail", {code: t, message: i[t]}))
            }
        }).on(this._handlers)
    }, t.prototype.process = function () {
        this.$element.addClass("process")
    }, t.prototype.update = function (t, i) {
        this.$element.removeClass("process"), this.$element.addClass("MF000" === i.code ? "success" : "fail"), setTimeout(e.proxy(function () {
            this.$element.trigger("reset")
        }, this), 3e3)
    }, t.prototype.reset = function () {
        this.$element.removeClass("success"), this.$element.removeClass("fail"), this.$element.trigger("mf.reset")
    }, e.fn.rdMailForm = function (i, o) {
        return this.each(function () {
            e(this).data("rdMailForm") || e(this).data("rdMailForm", new t(this, i, o))
        })
    }, e.fn.rdMailForm.Constructor = t
}(window.jQuery, window, document), function (e) {
    var t = e.fn.rdMailForm.Constructor.Plugins.Validator = function (i) {
        this._core = i, this._handlers = {
            "mfValidator.validate": this.validate,
            "mfValidator.error": this.error,
            "mfValidator.valid": this.valid,
            "mfValidator.reset": this.reset,
            "mfValidator.click": e.noop()
        }, this._core.options = e.extend(!0, {}, t.Defaults, this._core.options), this.initialize()
    };
    t.Defaults = {
        validator: {
            applyTo: "[data-constraints]",
            "class": "mfValidation",
            constraints: {
                "@LettersOnly": {
                    rule: "^([a-zA-Zа-яА-ЯіїёІЇЁєЄҐґ\\s]{0,})$",
                    message: "Please use letters only!"
                },
                "@NumbersOnly": {rule: "^-?\\d*\\.?\\d*$", message: "Please use numbers only!"},
                "@NotEmpty": {rule: "([^\\s])", message: "Field should not be empty!"},
                "@Email": {
                    rule: "^(([\\w-]+(?:\\.[\\w-]+)*)@((?:[\\w-]+\\.)*\\w[\\w-]{0,66})\\.([a-z]{2,6}(?:\\.[a-z]{2})?)){0,}$",
                    message: "Enter valid e-mail address!"
                },
                "@Phone": {
                    rule: "^(\\+?\\d{0,3}\\s*\\(?\\d{1,3}\\)?\\s*\\d{3}\\s*\\d{4}){0,}$",
                    message: "Enter valid phone number!"
                },
                "@Date": {
                    rule: function (e) {
                        return navigator.userAgent.match(/(iPod|iPhone|iPad)/) ? !0 : new RegExp("^($)|(((0[13578]|10|12)(-|\\/)((0[1-9])|([12])([0-9])|(3[01]?))(-|\\/)((19)([2-9])(\\d{1})|(20)([01])(\\d{1})|([8901])(\\d{1}))|(0?[2469]|11)(-|/)(([1-9])|(0[1-9])|([12])([0-9]?)|(3[0]?))(-|/)((19)([2-9])(\\d{1})|(20)([01])(\\d{1})|([8901])(\\d{1}))))$").test(e.val())
                    }, message: "Use MM/DD/YYYY format!"
                },
                "@SelectRequired": {
                    rule: function (e) {
                        return 0 !== e.find("option:selected").index()
                    }, message: "Please choose an option!"
                }
            }
        }
    }, t.prototype.initialize = function () {
        this._core.$element.trigger("mfValidator.initialize"), this.create(), this.watch(), this._core.$element.trigger("mfValidator.initialized")
    }, t.prototype.create = function () {
        var t = this;
        this._core.$element.find(this._core.options.validator.applyTo).each(function () {
            e(this).parent().append(e("<span/>", {"class": t._core.options.validator["class"]}))
        })
    }, t.prototype.watch = function () {
        var t = this;
        this._core.$element.find(this._core.options.validator.applyTo).on("keyup", function () {
            (e(this).is("input") || e(this).is("textarea")) && e(this).parent().find(".mfValidation").hasClass("error") && e(this).parent().trigger("mfValidator.validate", {options: t._core.options.validator})
        }).on("blur", function () {
            (e(this).is("input") || e(this).is("textarea")) && e(this).parent().trigger("mfValidator.validate", {options: t._core.options.validator})
        }).on("change", function () {
            e(this).is("select") && e(this).parent().trigger("mfValidator.validate", {options: t._core.options.validator})
        }).parent().on(this._handlers).find("." + this._core.options.validator["class"]).on("click", function () {
            e(this).removeClass("error").removeClass("show").addClass("hide").parent().trigger("mfValidator.click").find(t._core.options.validator.applyTo).focus()
        }), this._core.$element.on("submit", e.proxy(function (i) {
            return this._core.$element.find(this._core.options.validator.applyTo).each(function () {
                e(this).parent().trigger("mfValidator.validate", {options: t._core.options.validator})
            }), this._core.$element.find(".error").length ? (i.preventDefault(), !1) : void 0
        }, this)).on("mf.reset", e.proxy(function () {
            this._core.$element.find(this._core.options.validator.applyTo).each(function () {
                e(this).parent().trigger("mfValidator.reset", {options: t._core.options.validator})
            })
        }, this))
    }, t.prototype.validate = function (t, i) {
        var o, s = [], n = [], a = e(this), r = a.find(i.options.applyTo), l = r.data("constraints").match(/\@\w+/g), c = r.val();
        for (var d in l)if (i.options.constraints[l[d]]) {
            switch (typeof i.options.constraints[l[d]].rule) {
                case"function":
                    i.options.constraints[l[d]].rule(r) ? a.find(".mfValidation") && a.find(".mfValidation").attr("data-index") === d && (o = !0, a.find(".mfValidation").attr("data-index", -1)) : (s.push(i.options.constraints[l[d]].message), n.push(d), o = !0);
                    break;
                default:
                    new RegExp(i.options.constraints[l[d]].rule).test(c) ? a.find(".mfValidation").attr("data-index") && a.find(".mfValidation").attr("data-index") == d && (o = !0, a.find(".mfValidation").attr("data-index", -1)) : (s.push(i.options.constraints[l[d]].message), n.push(d), o = !0)
            }
            if (o)break
        }
        s.length ? e(this).trigger("mfValidator.error", {
            options: i.options,
            errors: s,
            indexes: n
        }) : e(this).trigger("mfValidator.valid", {options: i.options})
    }, t.prototype.error = function (t, i) {
        e(this).find("." + i.options["class"]).removeClass("valid").removeClass("hide").addClass("show").addClass("error").attr("data-index", i.indexes[0]).text(i.errors)
    }, t.prototype.valid = function (t, i) {
        var o = e(this).find("." + i.options["class"]);
        o.hasClass("error") && o.removeClass("error").addClass("hide"), o.find("." + i.options["class"]).removeClass("show").addClass("valid").text(i.errors)
    }, t.prototype.reset = function (t, i) {
        var o = e(this).find("." + i.options["class"]);
        o.hasClass("error") && o.removeClass("error").addClass("hide"), e(this).find("." + i.options["class"]).removeClass("show")
    }
}(window.jQuery, window, document), function (e) {
    var t = e.fn.rdMailForm.Constructor.Plugins.Input = function (i) {
        this._core = i, this._handlers = {
            "mfInput.focus": this.focus,
            "mfInput.blur": this.blur,
            "mfInput.type": this.type,
            "mfInput.delete": this["delete"],
            "mfInput.fill": this.fill,
            "mfInput.empty": this.empty,
            "mfInput.idle": this.idle,
            "mfInput.reset": this.reset,
            click: function (e) {
                return e.preventDefault(), !1
            }
        }, this._core.options = e.extend(!0, {}, t.Defaults, this._core.options), this.initialize()
    };
    t.Defaults = {
        input: {
            applyto: 'input[type="text"], input[type="date"], textarea',
            "class": "mfInput"
        }
    }, t.prototype.initialize = function () {
        this._core.$element.trigger("mfInput.initialize"), this.create(), this.watch(), this._core.$element.trigger("mfInput.initialized")
    }, t.prototype.create = function () {
        this._core.$element.find(this._core.options.input.applyto).parent().addClass(this._core.options.input["class"])
    }, t.prototype.watch = function () {
        this._core.$element.find(this._core.options.input.applyto).on("focus", function () {
            e(this).parent().trigger("mfInput.focus")
        }).on("blur", function () {
            e(this).parent().trigger("mfInput.blur"), "" === e(this).val() && e(this).parent().trigger("mfInput.void")
        }).on("keydown", this, function (t) {
            t.data.ignore(t) || ((8 === t.keyCode || 46 === t.keyCode) && e(this).parent().trigger("mfInput.delete"), (32 === t.keyCode || t.keyCode > 46) && e(this).parent().trigger("mfInput.type"))
        }).on("keyup", this, function (t) {
            var i = e(this);
            t.data.ignore(t) || ("" === i.val() && i.parent().trigger("mfInput.empty"), 8 === t.keyCode || 46 === t.keyCode ? (self.timer && clearTimeout(self.timer), self.timer = setTimeout(function () {
                i.parent().trigger("mfInput.idle")
            }, 1e3)) : (i.parent().trigger("mfInput.fill"), i.parent().trigger("mfInput.type"), self.timer && clearTimeout(self.timer), self.timer = setTimeout(function () {
                i.parent().trigger("mfInput.idle")
            }, 1e3)))
        }).on("keypress", this, function (t) {
            if (!t.data.ignore(t.keyCode)) {
                var i = e(this);
                self.timer && clearTimeout(self.timer), self.timer = setTimeout(function () {
                    i.parent().trigger("mfInput.idle")
                }, 1e3)
            }
        }).parent().on(this._handlers), this._core.$element.on("mf.reset", this, function (t) {
            e(this).find("." + t.data._core.options.input["class"]).each(function () {
                e(this).trigger("mfInput.reset")
            })
        })
    }, t.prototype.focus = function () {
        e(this).addClass("focused")
    }, t.prototype.blur = function () {
        e(this).removeClass("focused")
    }, t.prototype.type = function () {
        e(this).removeClass("deleting"), e(this).addClass("typing")
    }, t.prototype["delete"] = function () {
        e(this).removeClass("typing"), e(this).addClass("deleting")
    }, t.prototype.fill = function () {
        e(this).addClass("filled")
    }, t.prototype.empty = function () {
        e(this).removeClass("filled")
    }, t.prototype.idle = function () {
        e(this).removeClass("typing"), e(this).removeClass("deleting")
    }, t.prototype.reset = function () {
        e(this).removeClass("focused"), e(this).removeClass("deleting"), e(this).removeClass("filled"), e(this).removeClass("typing"), e(this).removeClass("error")
    }, t.prototype.ignore = function (e) {
        return 144 === e.keyCode || 20 === e.keyCode || 17 === e.keyCode || 37 === e.keyCode || 38 === e.keyCode || 39 === e.keyCode || 40 === e.keyCode || 112 === e.keyCode || 113 === e.keyCode || 114 === e.keyCode || 115 === e.keyCode || 116 === e.keyCode || 117 === e.keyCode || 118 === e.keyCode || 119 === e.keyCode || 120 === e.keyCode || 121 === e.keyCode || 122 === e.keyCode || 123 === e.keyCode || 9 === e.keyCode || e.ctrlKey ? !0 : !1
    }
}(window.jQuery, window, document), function (e) {
    var t = e.fn.rdMailForm.Constructor.Plugins.Select = function (i) {
        this._core = i, this._handlers = {
            "mfSelect.close": this.close,
            "mfSelect.open": this.open,
            "mfSelect.select": this.select,
            click: function (e) {
                e.preventDefault(), e.stopPropagation()
            }
        }, this._core.options = e.extend(!0, {}, t.Defaults, this._core.options), this.initialize()
    };
    t.Defaults = {select: {applyTo: "select", "class": "mfSelect"}}, t.prototype.initialize = function () {
        this._core.$element.trigger("mfSelect.initialize"), this.create(), this.watch(), this._core.$element.trigger("mfSelect.initialized")
    }, t.prototype.create = function () {
        this._core.$element.find(this._core.options.select.applyTo).each(function () {
            var t = e(this);
            t.css({
                position: "absolute",
                left: "50%",
                width: "0",
                height: "0",
                overflow: "hidden",
                opacity: "0"
            }).parent().append(e("<div/>", {
                "class": "value",
                text: t.find("option:selected").text()
            })).append(e("<ul/>", {"class": "dropdown"})).end().find("option").each(function (t) {
                if (0 != t) {
                    var i = e(this);
                    i.parent().parent().find(".dropdown").append(e("<li/>", {
                        "class": "option",
                        text: i.text()
                    }).addClass(i.is(":selected") ? "selected" : ""))
                }
            })
        }).parent().addClass(this._core.options.select["class"])
    }, t.prototype.watch = function () {
        var t = this;
        this._core.$element.find(t._core.options.select.applyTo).on("focus", this.focus).on("blur", function () {
            e(this).parent().trigger("mfSelect.close").removeClass("focus")
        }).on("keydown", function (t) {
            38 == t.keyCode && e(this).val(e(this).find("option").eq(e(this).find("option:selected").index() > 0 ? e(this).find("option:selected").index() - 1 : 0).text()).trigger("change"), 40 == t.keyCode && e(this).val(e(this).find("option").eq(e(this).find("option:selected").index() < e(this).find("option").length - 1 ? e(this).find("option:selected").index() + 1 : e(this).find("option").length - 1).text()).trigger("change"), 13 == t.keyCode && e(this).parent().trigger(e(this).parent().hasClass("show") ? "mfSelect.close" : "mfSelect.open"), (32 == t.keyCode || 37 == t.keyCode || 38 == t.keyCode || 39 == t.keyCode || 40 == t.keyCode || 13 == t.keyCode) && t.preventDefault()
        }).on("change", function () {
            e(this).parent().trigger("mfSelect.open").find(".value").text(e(this).val());
            var t = e(this).find("option:selected").index(), i = e(this).parent().find(".option").removeClass("selected");
            t > 0 && i.eq(t - 1).addClass("selected")
        }).parent().on(this._handlers).find(".value").on("click", function () {
            var i = e(this), o = i.parent().find("select"), s = o.find("option").eq(0).text();
            if (i.text(s), o.trigger("focus").off("focus", t.focus), !e(this).parent().hasClass("show")) {
                o.on("focus", t.focus);
                var n = e(this).parent().find(".option.selected");
                n.length && i.text(n.text())
            }
        }).parent().find(".option").on("click", function () {
            e(this).parent().find(".option").removeClass("selected"), e(this).addClass("selected"), e(this).parent().parent().find("select").focus().on("focus", t.focus), e(this).parent().parent().trigger("mfSelect.select", {
                options: t._core.options.select,
                value: e(this).text()
            })
        }).parents("body").on("click", function (i) {
            var o = t._core.$element.find("." + t._core.options.select["class"]);
            o.length && (o.is(i.target) || 0 !== o.has(i.target).length || o.find("select").each(function () {
                var t = e(this).parent().find(".option.selected");
                t.length && e(this).parent().find(".value").text(t.text())
            }).on("focus", t.focus))
        }), this._core.$element.on("mf.reset", function () {
            e(this).find(t._core.options.select.applyTo).each(function () {
                e(this).parent().find(".value").text(e(this).prop("selectedIndex", 0).val()), e(this).parent().find(".option").removeClass("selected")
            })
        })
    }, t.prototype.focus = function () {
        e(this).parent().trigger("mfSelect.open").addClass("focus")
    }, t.prototype.close = function () {
        navigator.userAgent.match(/(iPod|iPhone|iPad)/) || e(this).hasClass("show") && e(this).removeClass("show")
    }, t.prototype.open = function () {
        navigator.userAgent.match(/(iPod|iPhone|iPad)/) || e(this).hasClass("show") || e(this).addClass("show")
    }, t.prototype.select = function (t, i) {
        e(this).find(i.options.applyTo).val(i.value).trigger("change"), e(this).trigger("mfSelect.close")
    }
}(window.jQuery, window, document), function (e) {
    var t = e.fn.rdMailForm.Constructor.Plugins.DatePicker = function (i) {
        this._core = i, this._handlers = {
            "mfDatePicker.close": this.close,
            "mfDatePicker.open": this.open,
            "mfDatePicker.next": this.next,
            "mfDatePicker.prev": this.prev,
            "mfDatePicker.update": this.update,
            "mfDatePicker.refresh": this.refresh,
            "mfDatePicker.pick": this.pick
        }, this._core.options = e.extend(!0, {}, t.Defaults, this._core.options), this.initialize()
    };
    t.Defaults = {
        datepicker: {
            applyTo: 'input[type="date"]',
            "class": "mfDatePicker",
            days: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
            months: ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
            format: "MM-DD-YYYY",
            prevMonth: "",
            nextMonth: ""
        }
    }, t.prototype.initialize = function () {
        this._core.$element.trigger("mfDatePicker.initialize"), this.create(), this.watch(), this._core.$element.trigger("mfDatePicker.initialized")
    }, t.prototype.create = function () {
        var t = this;
        t._core.$element.find(t._core.options.datepicker.applyTo).each(function () {
            e(this).attr({
                type: navigator.userAgent.match(/(iPod|iPhone|iPad)/) ? "date" : "text",
                "data-type": "date"
            }).after(e("<div/>", {"class": t._core.options.datepicker["class"]}).data("date", new Date))
        }).parent().find("." + t._core.options.datepicker["class"]).each(function () {
            e.proxy(t.update, this, {}, t._core.options.datepicker).call(), e.proxy(t.refresh, this, {}, t._core.options.datepicker).call()
        })
    }, t.prototype.watch = function () {
        var t = this;
        t._core.$element.find("." + t._core.options.datepicker["class"]).on("click", "." + t._core.options.datepicker["class"] + "_next", function () {
            var i = e(this).parents("." + t._core.options.datepicker["class"]);
            i.trigger("mfDatePicker.next"), i.trigger("mfDatePicker.update", t._core.options.datepicker), i.trigger("mfDatePicker.refresh", t._core.options.datepicker)
        }).on("click", "." + t._core.options.datepicker["class"] + "_prev", function () {
            var i = e(this).parents("." + t._core.options.datepicker["class"]);
            i.trigger("mfDatePicker.prev"), i.trigger("mfDatePicker.update", t._core.options.datepicker), i.trigger("mfDatePicker.refresh", t._core.options.datepicker)
        }).on("click", ".dp-day", function () {
            var i = e(this).parents("." + t._core.options.datepicker["class"]);
            i.trigger("mfDatePicker.pick", {
                opt: t._core.options.datepicker,
                day: e(this)
            }), i.parent().find("input").on("blur", t.blur).trigger("blur").trigger("keyup")
        }).on("click", function () {
        }).on(this._handlers).parent().on("click", function (e) {
            return e.preventDefault(), !1
        }).find("input").on("focus", function () {
            e(this).parent().find("." + t._core.options.datepicker["class"]).trigger("mfDatePicker.open")
        }).on("blur", this.blur).on("keydown", function (i) {
            (9 == i.keyCode || i.shiftKey && 9 == i.keyCode) && e(this).on("blur", t.blur)
        }).parents("body").on("mousedown", function (e) {
            var i = t._core.$element.find("." + t._core.options.datepicker["class"]).parent();
            i.length && (i.is(e.target) || 0 !== i.has(e.target).length ? i.find("input").off("blur", t.blur) : i.find("input").on("blur", t.blur).trigger("blur"))
        }), t._core.$element.on("mf.reset", function () {
            e(this).find("." + t._core.options.datepicker["class"]).each(function () {
                e(this).trigger("mfDatePicker.close")
            })
        })
    }, t.prototype.blur = function () {
        e(this).parent().find(".mfDatePicker").trigger("mfDatePicker.close")
    }, t.prototype.close = function () {
        navigator.userAgent.match(/(iPod|iPhone|iPad)/) || e(this).hasClass("open") && e(this).removeClass("open")
    }, t.prototype.open = function () {
        navigator.userAgent.match(/(iPod|iPhone|iPad)/) || e(this).hasClass("open") || e(this).addClass("open")
    }, t.prototype.next = function () {
        var t = e(this), i = t.data("date");
        i = 11 == i.getMonth() ? new Date(i.getFullYear() + 1, 0, 1) : new Date(i.getFullYear(), i.getMonth() + 1, 1), t.data("date", i)
    }, t.prototype.prev = function () {
        var t = e(this), i = t.data("date");
        i = 0 == i.getMonth() ? new Date(i.getFullYear() - 1, 11, 1) : new Date(i.getFullYear(), i.getMonth() - 1, 1), t.data("date", i)
    }, t.prototype.pick = function (t, i) {
        var o = e(this);
        o.data("pickedDate", i.day.addClass("dp-selected").data("date")), o.find(".dp-day").not(i.day).removeClass("dp-selected"), o.parent().find("input").val((o.data("pickedDate").getMonth() + 1 < 10 ? "0" + (o.data("pickedDate").getMonth() + 1) : o.data("pickedDate").getMonth() + 1) + "/" + (o.data("pickedDate").getDate() < 10 ? "0" + o.data("pickedDate").getDate() : o.data("pickedDate").getDate()) + "/" + o.data("pickedDate").getFullYear())
    }, t.prototype.update = function (t, i) {
        var o = e(this), s = e("<div/>", {"class": i["class"] + "_panel"});
        s.append(e("<a/>", {
            "class": i["class"] + "_prev",
            text: i.prevMonth
        })), s.append(e("<a/>", {
            "class": i["class"] + "_next",
            text: i.nextMonth
        })), s.append(e("<div/>", {
            "class": i["class"] + "_title",
            text: i.months[o.data("date").getMonth()] + " " + o.data("date").getFullYear()
        }));
        var n = o.find("." + i["class"] + "_panel");
        n.length ? n.replaceWith(s) : s.appendTo(o)
    }, t.prototype.refresh = function (t, i) {
        for (var o = e(this), s = e("<table/>"), n = e("<tr/>"), a = 0; a < i.days.length; a++)n.append(e("<th/>", {
            "class": "dp-weekday",
            text: i.days[a]
        }));
        s.append(n);
        for (var r = o.data("date"), l = o.data("pickedDate"), c = new Date(r.getFullYear(), r.getMonth() + 1, 0).getDate(), d = new Date(r.getFullYear(), r.getMonth(), 0).getDate(), p = new Date(r.getFullYear(), r.getMonth(), 1).getDay(), f = 1, a = 0; 7 > a; a++) {
            n = e("<tr/>");
            for (var h = 0; 7 > h; h++) {
                var u, m = 7 * a + h + 1, g = e("<td/>", {"class": "dp-day"}), y = new Date;
                if (y.setHours(0), y.setMinutes(0), y.setSeconds(0), y.setMilliseconds(0), 0 == h && m > c + p)break;
                1 > m - p ? (g.text(d + (m - p)).addClass("dp-offset"), u = new Date(r.getFullYear(), r.getMonth() - 1, d + (m - p))) : c + p >= m ? (g.text(m - p), u = new Date(r.getFullYear(), r.getMonth(), m - p)) : (g.text(f).addClass("dp-offset"), u = new Date(r.getFullYear(), r.getMonth() + 1, f++)), u.valueOf() == y.valueOf() && g.addClass("dp-today"), l && u.valueOf() == l.valueOf() && g.addClass("dp-selected"), n.append(g.data("date", u))
            }
            "" != n.html() && s.append(n)
        }
        var v = o.find("table");
        v.length ? v.replaceWith(s) : s.appendTo(o)
    }
}(window.jQuery, window, document), function (e) {
    var t = e.fn.rdMailForm.Constructor.Plugins.Icon = function (i) {
        this._core = i, this._core.options = e.extend(!0, {}, t.Defaults, this._core.options), this.initialize()
    };
    this._handlers = {"mfIcon.change": this.change}, t.Defaults = {
        icon: {
            applyTo: "[data-add-icon]",
            "class": "mfIcon",
            states: {
                ".mfInput": {
                    "mfIcon.default": ["mfInput.blur", "mfInput.idle", "mfInput.reset"],
                    "mfIcon.state-1": ["mfInput.type"],
                    "mfIcon.state-2": ["mfInput.delete"]
                }
            }
        }
    }, t.prototype.initialize = function () {
        this._core.$element.trigger("mfIcon.initialize"), this.create(), this.watch(), this._core.$element.trigger("mfIcon.initialized")
    }, t.prototype.create = function () {
        var t = this;
        t._core.$element.find(t._core.options.icon.applyTo).each(function () {
            var i = e(this);
            i.append(e("<span/>", {"class": t._core.options.icon["class"]}).append(e("<span/>")))
        })
    }, t.prototype.watch = function () {
        var t = this;
        t._core.$element.find("." + t._core.options.icon["class"]).on(t._handlers);
        for (var i in t._core.options.icon.states) {
            var o = t._core.$element.find(i);
            for (var s in t._core.options.icon.states[i])for (var n in t._core.options.icon.states[i][s])o.on(t._core.options.icon.states[i][s][n], {state: s}, function (i) {
                e(this).find("." + t._core.options.icon["class"]).attr("class", i.data.state.replace(".", " "))
            })
        }
    }
}(window.jQuery, window, document), function (e) {
    var t = e.fn.rdMailForm.Constructor.Plugins.Placeholder = function (i) {
        this._core = i, this._core.options = e.extend(!0, {}, t.Defaults, this._core.options), this.initialize()
    };
    this._handlers = {"mfIcon.change": this.change}, t.Defaults = {
        placeholder: {
            applyTo: "[data-add-placeholder]",
            "class": "mfPlaceHolder",
            states: {
                ".mfInput": {
                    "mfPlaceHolder.default": ["mfInput.void", "mfInput.reset"],
                    "mfPlaceHolder.state-1": ["mfInput.fill", "mfInput.focus"]
                }
            }
        }
    }, t.prototype.initialize = function () {
        this._core.$element.trigger("mfPlaceHolder.initialize"), this.create(), this.watch(), this._core.$element.trigger("mfPlaceHolder.initialized")
    }, t.prototype.create = function () {
        var t = this;
        t._core.$element.find(t._core.options.placeholder.applyTo).each(function () {
            var i = e(this);
            i.append(e("<span/>", {
                "class": t._core.options.placeholder["class"],
                text: i.find("[placeholder]").attr("placeholder") ? i.find("[placeholder]").attr("placeholder") : i.find("[data-placeholder]").attr("data-placeholder")
            })).find("[placeholder]").removeAttr("placeholder").removeAttr("data-placeholder")
        })
    }, t.prototype.watch = function () {
        var t = this;
        t._core.$element.find("." + t._core.options.placeholder["class"]).on("click", function () {
            e(this).parent().find("input, textarea").trigger("focus")
        }).on(t._handlers);
        for (var i in t._core.options.icon.states) {
            var o = t._core.$element.find(i);
            for (var s in t._core.options.placeholder.states[i])for (var n in t._core.options.placeholder.states[i][s])o.on(t._core.options.placeholder.states[i][s][n], {state: s}, function (i) {
                e(this).find("." + t._core.options.placeholder["class"]).attr("class", i.data.state.replace(".", " "))
            })
        }
    }
}(window.jQuery, window, document), function (e) {
    var t = e.fn.rdMailForm.Constructor.Plugins.Progress = function (i) {
        this._core = i, this._core.options = e.extend(!0, {}, t.Defaults, this._core.options), this.initialize()
    };
    t.Defaults = {progress: {applyTo: ".mfInfo", "class": "mfProgress"}}, t.prototype.initialize = function () {
        this._core.$element.trigger("mfProgress.initialize"), this.create(), this.watch(), this._core.$element.trigger("mfProgress.initialized")
    }, t.prototype.create = function () {
        var t = this;
        t._core.$element.find(t._core.options.progress.applyTo).each(function () {
            var i = e(this);
            i.addClass(t._core.options.progress["class"]).wrapInner(e("<span/>", {"class": "cnt"})).append(e("<span/>", {"class": "loader"})).append(e("<span/>", {"class": "msg"}))
        })
    }, t.prototype.watch = function () {
        var t = this;
        t._core.$element.on("mf.process", function () {
            e(this).find("." + t._core.options.progress["class"]).removeClass("hide").addClass("sending").find(".msg").text("Loading...")
        }).on("mf.fail", function (i, o) {
            e(this).find("." + t._core.options.progress["class"]).removeClass("sending").addClass("fail").find(".msg").text(o.message), setTimeout(e.proxy(function () {
                e(this).find("." + t._core.options.progress["class"]).removeClass("fail").addClass("hide").find(".msg")
            }, this), 3e3)
        }).on("mf.success", function (i, o) {
            e(this).find("." + t._core.options.progress["class"]).removeClass("sending").addClass("success").find(".msg").text(o.message), setTimeout(e.proxy(function () {
                e(this).find("." + t._core.options.progress["class"]).removeClass("success").addClass("hide").find(".msg")
            }, this), 1500)
        }).on("mf.reset", function () {
            e(this).find("." + t._core.options.progress["class"]).removeClass("sending").removeClass("fail").removeClass("success").find(".msg")
        })
    }
}(window.jQuery, window, document);