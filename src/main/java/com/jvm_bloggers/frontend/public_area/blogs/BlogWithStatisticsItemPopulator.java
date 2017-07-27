package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListing;

import com.jvm_bloggers.frontend.public_area.blogs.single_blog.BlogPostsPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.frontend.public_area.blogs.single_blog.BlogPostsPage.BLOG_BOOKMARKABLE_ID_PARAM;
import static org.apache.commons.lang3.StringUtils.abbreviate;

@Component
public class BlogWithStatisticsItemPopulator {

    private static final String TWITTER_HOME_URL = "https://twitter.com/";
    static final String BLOG_POSTS_LINK_ID = "blogPostsLinkId";
    static final String AUTHOR_BLOG_LINK_ID = "authorLink";
    static final String AUTHOR_ID = "author";
    static final String LINK_LABEL_ID = "linkLabelId";
    static final String FIRST_COUNTER_ID = "countFirstRange";
    static final String SECOND_COUNTER_ID = "countSecondRange";

    public void populateItem(final Item<BlogStatisticsForListing> item) {
        BlogStatisticsForListing blogStatisticsForListing = item.getModelObject();
        BookmarkablePageLink blogPostsLink = new BookmarkablePageLink<>(
            BLOG_POSTS_LINK_ID,
            BlogPostsPage.class,
            new PageParameters()
                .add(BLOG_BOOKMARKABLE_ID_PARAM, blogStatisticsForListing.getBookmarkableId()));
        blogPostsLink.add(new Label(LINK_LABEL_ID,
            abbreviate(blogStatisticsForListing.getUrl(), 40)));
        item.add(blogPostsLink);
        item.add(new ExternalLink(AUTHOR_BLOG_LINK_ID,
            TWITTER_HOME_URL + blogStatisticsForListing.getTwitter().getOrElse(""),
            blogStatisticsForListing.getAuthor())
            .setVisible(blogStatisticsForListing.getTwitter().isDefined()));
        item.add(new Label(AUTHOR_ID, blogStatisticsForListing.getAuthor())
            .setVisible(blogStatisticsForListing.getTwitter().isEmpty()));
        Label firstCount = new Label(FIRST_COUNTER_ID,
            blogStatisticsForListing.getCountFirstRange());
        firstCount.setVisible(displayCounters(blogStatisticsForListing));
        item.add(firstCount);
        Label secondCount = new Label(SECOND_COUNTER_ID,
            blogStatisticsForListing.getCountSecondRange());
        secondCount.setVisible(displayCounters(blogStatisticsForListing));
        item.add(secondCount);
    }

    private boolean displayCounters(BlogStatisticsForListing blogStatisticsForListing) {
        return blogStatisticsForListing.getCountSecondRange() > 0;
    }
}
