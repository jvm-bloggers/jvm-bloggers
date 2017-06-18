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

}
