package com.jvm_bloggers.core.newsletter_issues;

class PublishingConstants {

    private PublishingConstants() {
        throw new IllegalStateException("Utility class");
    }

    public static final int DAYS_IN_THE_PAST_TO_INCLUDE_IN_NEW_ISSUE = 7;
    public static final int STARTING_HOUR_TO_INCLUDE_IN_NEW_ISSUE = 7;

}
