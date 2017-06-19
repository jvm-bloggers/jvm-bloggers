package com.jvm_bloggers.entities.blog.projections;

public interface BlogStatisticsProjection {

    Long getId();

    String getUrl();

    String getAuthor();

    String getTwitter();

    Integer getFirstCount();

    Integer getSecondCount();

}
