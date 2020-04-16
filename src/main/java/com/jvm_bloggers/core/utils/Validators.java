package com.jvm_bloggers.core.utils;

import lombok.experimental.UtilityClass;

import org.apache.commons.validator.routines.UrlValidator;
import org.springframework.util.StringUtils;

@UtilityClass
public class Validators {

    private static final UrlValidator URL_VALIDATOR =
        new UrlValidator(new String[]{"http", "https"});

    public static boolean isUrlValid(String url) {

        boolean isValid = URL_VALIDATOR.isValid(url);

        // maybe it is the problem with 2nd '//' in address e.g. http://example.com//something
        if (!isValid && oneNotNeededDoubleSlashIsPresent(url)) {
            String urlWithoutDoubleSlash = reverseThenReplaceDoubleSlashThenReverseBack(url);
            return URL_VALIDATOR.isValid(urlWithoutDoubleSlash);
        }

        return isValid;
    }

    private static String reverseThenReplaceDoubleSlashThenReverseBack(String url) {
        String reversedUrlWithReplacedDoubleSlash = new StringBuilder(url)
            .reverse()
            .toString()
            .replaceFirst("//", "/");
        return new StringBuilder(reversedUrlWithReplacedDoubleSlash)
            .reverse()
            .toString();
    }

    private static boolean oneNotNeededDoubleSlashIsPresent(String url) {
        return StringUtils.countOccurrencesOf(url, "//") == 2;
    }

}
