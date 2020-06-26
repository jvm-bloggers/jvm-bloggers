package com.jvm_bloggers.utils;

import lombok.experimental.UtilityClass;

import org.jsoup.Jsoup;

@UtilityClass
public class HtmlEmptinessChecker {

    public static boolean isNotEmpty(String htmlContent) {
        return htmlContent != null && Jsoup
            .parse(
                htmlContent.replace("&nbsp;", " ")
            )
            .hasText();
    }

    public static boolean isEmpty(String htmlContent) {
        return !isNotEmpty(htmlContent);
    }

}
