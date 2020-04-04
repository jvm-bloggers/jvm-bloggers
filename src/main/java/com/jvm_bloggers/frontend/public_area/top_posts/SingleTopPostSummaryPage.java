package com.jvm_bloggers.frontend.public_area.top_posts;

import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedPost;
import com.jvm_bloggers.domain.query.top_posts_summary.PublishedTopPostSummary;
import com.jvm_bloggers.frontend.common_components.PublishedBlogPostLink;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;
import com.jvm_bloggers.frontend.public_area.social_meta_data.SocialMetaData;
import io.vavr.control.Try;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.time.YearMonth;

import static com.jvm_bloggers.frontend.public_area.top_posts.SingleTopPostSummaryPage.MONTH_URL_PLACEHOLDER;
import static com.jvm_bloggers.frontend.public_area.top_posts.SingleTopPostSummaryPage.YEAR_URL_PLACEHOLDER;
import static com.jvm_bloggers.utils.DateTimeUtilities.stringify;
import static java.lang.String.format;

@MountPath("/top-articles/${" + YEAR_URL_PLACEHOLDER + "}/${" +  MONTH_URL_PLACEHOLDER + "}")
@Slf4j
public class SingleTopPostSummaryPage extends AbstractFrontendPage {

    static final String YEAR_URL_PLACEHOLDER = "year";
    static final String MONTH_URL_PLACEHOLDER = "month";

    static final String TOP_PERSONAL_POSTS_ID = "topPersonalPosts";
    static final String BLOG_POST_LINK_ID = "blogPostLink";
    static final String TOP_COMPANY_POSTS_ID = "topCompanyPosts";

    @SpringBean
    private SingleTopPostSummaryPageBackingBean backingBean;

    public SingleTopPostSummaryPage(PageParameters parameters) {
        super(parameters);
        findSummary(parameters)
            .onSuccess(this::buildView)
            .onFailure(t -> {
                setResponsePage(TopPostsPage.class);
                log.warn("Summary not found for " + parameters);
            });
    }

    private Try<PublishedTopPostSummary> findSummary(PageParameters parameters) {
        return parseParameters(parameters)
            .map(yearMonth -> backingBean.findSummaryFor(yearMonth).get());
    }

    private Try<YearMonth> parseParameters(PageParameters params) {
        return Try.of(() ->
            YearMonth.of(
                params.get(YEAR_URL_PLACEHOLDER).toInt(),
                params.get(MONTH_URL_PLACEHOLDER).toInt()
            )
        );
    }

    private void buildView(PublishedTopPostSummary summary) {
        add(new Label("headerLabel", stringify(summary.getYearMonth()) + " - Najlepsze posty"));

        add(new ListView<PublishedPost>(
            TOP_PERSONAL_POSTS_ID,
            summary.getTopPersonalPosts().toJavaList()
        ) {
            @Override
            protected void populateItem(ListItem<PublishedPost> item) {
                item.add(new PublishedBlogPostLink(BLOG_POST_LINK_ID, item.getModelObject()));
            }
        });

        add(new ListView<>(
            TOP_COMPANY_POSTS_ID,
            summary.getTopCompanyPosts().toJavaList()
        ) {
            @Override
            protected void populateItem(ListItem<PublishedPost> item) {
                item.add(new PublishedBlogPostLink(BLOG_POST_LINK_ID, item.getModelObject()));
            }
        });

    }

    public static PageParameters buildPageParams(YearMonth yearMonth) {
        return new PageParameters()
            .add(YEAR_URL_PLACEHOLDER, yearMonth.getYear())
            .add(MONTH_URL_PLACEHOLDER, format("%02d",yearMonth.getMonthValue()));
    }

    @Override
    protected String getPageTitle() {
        YearMonth month = extractYearMonth(getPageParameters());
        return String.format("%s - %s", stringify(month), "najlepsze posty");
    }

    private YearMonth extractYearMonth(PageParameters pageParameters) {
        return parseParameters(pageParameters).getOrElseGet(t -> YearMonth.now());
    }

    @Override
    protected SocialMetaData getSocialMetaTags() {
        return backingBean.createSocialMetaTags(extractYearMonth(getPageParameters()));
    }
}
