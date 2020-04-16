package com.jvm_bloggers.frontend.common_components.toastr;

import lombok.experimental.UtilityClass;

import static java.lang.String.format;

@UtilityClass
public class ToastrBuilder {

    private static final String command = ""
        + "toastr.%s('%s', null, {"
        + " 'positionClass': 'toast-top-center',"
        + " 'closeButton': true,"
        + " 'body-output-type': 'trustedHtml'"
        + "})";

    public static String error(String message) {
        return format(command, "error", message);
    }

    public static String info(String message) {
        return format(command, "info", message);
    }

    public static String success(String message) {
        return format(command, "success", message);
    }

    public static String warning(String message) {
        return format(command, "warning", message);
    }
}
