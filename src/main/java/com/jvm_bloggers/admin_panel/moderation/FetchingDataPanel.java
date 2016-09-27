package com.jvm_bloggers.admin_panel.moderation;

import com.jvm_bloggers.admin_panel.panels.CustomFeedbackPanel;
import com.jvm_bloggers.core.data_fetching.blog_posts.BlogPostsFetchingScheduler;
import com.jvm_bloggers.core.data_fetching.blogs.BloggersDataFetchingScheduler;
import com.jvm_bloggers.core.metadata.MetadataKeys;
import com.jvm_bloggers.core.metadata.MetadataRepository;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

@Slf4j
public class FetchingDataPanel extends Panel {

    @SpringBean
    private BloggersDataFetchingScheduler bloggersDataFetchingScheduler;

    @SpringBean
    private BlogPostsFetchingScheduler blogPostsFetchingScheduler;

    private static final ThreadPoolExecutor
        fetchingBlogPostExecutor = new ThreadPoolExecutor(1, 1,
        0L, TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue<>());

    private static final ThreadPoolExecutor
        fetchingBloggersDataExecutor = new ThreadPoolExecutor(1, 1,
        0L, TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue<>());

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

    private AjaxLink createFetchBlogPostsDataButton(CustomFeedbackPanel feedback) {
        return new AjaxLink("fetchBlogPostsData") {

            @Override
            public void onClick(AjaxRequestTarget target) {
                log.debug("Fetching blog posts data clicked");
                if (fetchingBlogPostExecutor.getActiveCount() == 0) {
                    fetchingBlogPostExecutor
                        .submit(blogPostsFetchingScheduler::checkRssForNewBlogPosts);
                    getSession().success("Fetching blog posts data started");
                } else {
                    getSession().error("Fetching blog posts data already in progress");
                }

                target.add(feedback);
            }
        };
    }


    private AjaxLink createFetchBloggersDataButton(CustomFeedbackPanel feedback) {

        return new AjaxLink("fetchBloggersData") {

            @Override
            public void onClick(AjaxRequestTarget target) {
                log.debug("Fetching bloggers data clicked");
                if (fetchingBloggersDataExecutor.getActiveCount() == 0) {
                    fetchingBloggersDataExecutor
                        .submit(bloggersDataFetchingScheduler::fetchBloggersData);
                    getSession().success("Fetching bloggers data started");
                } else {
                    getSession().error("Fetching bloggers data already in progress");
                }

                target.add(feedback);
            }
        };
    }
}
