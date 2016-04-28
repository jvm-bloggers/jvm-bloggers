package pl.tomaszdziurko.jvm_bloggers.utils;

import org.apache.commons.validator.routines.UrlValidator;

public class Validators {

    private static final UrlValidator URL_VALIDATOR =
        new UrlValidator(new String[]{"http", "https"});

    public static boolean isUrlValid(String url) {
        return URL_VALIDATOR.isValid(url);
    }

}
