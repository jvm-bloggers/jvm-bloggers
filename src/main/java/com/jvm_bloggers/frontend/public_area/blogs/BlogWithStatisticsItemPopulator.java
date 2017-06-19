package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListing;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.frontend.public_area.blogs.BlogPostsPage.BLOG_ID_PARAM;
import static org.apache.commons.lang3.StringUtils.abbreviate;

@Component
public class BlogWithStatisticsItemPopulator {

    private static final String TWITTER_HOME_URL = "https://twitter.com/";
    static final String URL_ID = "url";
    static final String BLOG_POSTS_LINK_ID = "blogPostsLinkId";
    static final String AUTHOR_BLOG_LINK_ID = "authorLink";
    static final String AUTHOR_ID = "author";
    static final String FIRST_COUNTER_ID = "countFirstRange";
    static final String SECOND_COUNTER_ID = "countSecondRange";

    public void populateItem(final Item<BlogStatisticsForListing> item) {
        BlogStatisticsForListing blogStatisticsForListing = item.getModelObject();
        item.add(new BookmarkablePageLink<>(
            BLOG_POSTS_LINK_ID,
            BlogPostsPage.class,
            new PageParameters()
                .add(BLOG_ID_PARAM, blogStatisticsForListing.getId())));
        item.add(new ExternalLink(URL_ID, blogStatisticsForListing.getUrl(),
            abbreviate(blogStatisticsForListing.getUrl(), 40)));
        item.add(new ExternalLink(AUTHOR_BLOG_LINK_ID,
            TWITTER_HOME_URL + blogStatisticsForListing.getTwitter().getOrElse(""),
            blogStatisticsForListing.getAuthor())
            .setVisible(blogStatisticsForListing.getTwitter().isDefined()));
        item.add(new Label(AUTHOR_ID, blogStatisticsForListing.getAuthor())
            .setVisible(blogStatisticsForListing.getTwitter().isEmpty()));
        item.add(new Label(FIRST_COUNTER_ID, blogStatisticsForListing.getCountFirstRange()));
        item.add(new Label(SECOND_COUNTER_ID, blogStatisticsForListing.getCountSecondRange()));
    }
}
