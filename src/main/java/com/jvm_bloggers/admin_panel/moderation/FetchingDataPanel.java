package com.jvm_bloggers.admin_panel.moderation;

import com.jvm_bloggers.admin_panel.panels.CustomFeedbackPanel;
import com.jvm_bloggers.core.data_fetching.blog_posts.BlogPostsFetcher;
import com.jvm_bloggers.core.data_fetching.blogs.BloggersDataFetcher;
import com.jvm_bloggers.core.metadata.MetadataKeys;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class FetchingDataPanel extends Panel {

    @SpringBean
    private BloggersDataFetcher bloggersDataFetcher;

    @SpringBean
    private BlogPostsFetcher blogPostsFetcher;

    private static final String
        FETCHING_DATA_PANEL_WICKET_ID =
        "fetchingDataPanel";

    FetchingDataPanel(CustomFeedbackPanel feedback) {
        super(FETCHING_DATA_PANEL_WICKET_ID);

        add(new Label("dateOfLastFetchingBlogPosts",
            new DateOfLastFetchingDataModel(MetadataKeys.DATE_OF_LAST_FETCHING_BLOG_POSTS)));
        add(createFetchBlogPostsDataButton(feedback));

        add(new Label("dateOfLastFetchingBloggers",
            new DateOfLastFetchingDataModel(MetadataKeys.DATE_OF_LAST_FETCHING_BLOGGERS)));
        add(createFetchBloggersDataButton(feedback));
    }

    private AjaxLink<Void> createFetchBlogPostsDataButton(CustomFeedbackPanel feedback) {
        return new AjaxLink<Void>("fetchBlogPostsData") {

            @Override
            public void onClick(AjaxRequestTarget target) {
                log.debug("Fetching blog posts data clicked");
                if (!blogPostsFetcher.isFetchingProcessInProgress()) {
                    blogPostsFetcher.refreshPosts();
                    getSession().success("Fetching blog posts data started");
                } else {
                    getSession().error("Fetching blog posts data already in progress");
                }

                target.add(feedback);
            }
        };
    }

    private AjaxLink<Void> createFetchBloggersDataButton(CustomFeedbackPanel feedback) {

        return new AjaxLink<Void>("fetchBloggersData") {

            @Override
            public void onClick(AjaxRequestTarget target) {
                log.debug("Fetching bloggers data clicked");
                if (!bloggersDataFetcher.isFetchingProcessInProgress()) {
                    bloggersDataFetcher.refreshData();
                    getSession().success("Fetching bloggers data started");
                } else {
                    getSession().error("Fetching bloggers data already in progress");
                }

                target.add(feedback);
            }
        };
    }
}
