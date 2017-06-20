package com.jvm_bloggers.utils;

import org.jsoup.Jsoup;

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
