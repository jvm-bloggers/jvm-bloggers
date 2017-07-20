package com.jvm_bloggers.entities.blog.projections;

public interface BlogStatisticsProjection {

    String getBookmarkableId();

    String getUrl();

    String getAuthor();

    String getTwitter();

    Integer getFirstCount();

    Integer getSecondCount();

}
