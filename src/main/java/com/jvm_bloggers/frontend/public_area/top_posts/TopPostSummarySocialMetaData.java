package com.jvm_bloggers.frontend.public_area.top_posts;

import com.jvm_bloggers.frontend.public_area.social_meta_data.DefaultSocialMetaData;

import java.time.YearMonth;

class TopPostSummarySocialMetaData extends DefaultSocialMetaData {

    private static final String TOP_POSTS_IMAGES_DIR = "/assets/images/top_post_pages";

    TopPostSummarySocialMetaData(YearMonth yearMonth, String applicationBaseUrl) {
        super(
            DEFAULT_TITLE,
            DEFAULT_DESCRIPTION,
            calculateImageName(yearMonth, applicationBaseUrl)
        );
    }

    private static String calculateImageName(YearMonth yearMonth, String applicationBaseUrl) {
        return applicationBaseUrl + TOP_POSTS_IMAGES_DIR
            + String.format("/month-%02d.png", yearMonth.getMonthValue());
    }

}
