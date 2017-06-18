package com.jvm_bloggers.frontend.common_components;

import com.jvm_bloggers.frontend.public_area.top_posts.SingleTopPostSummaryPage;
import com.jvm_bloggers.utils.DateTimeUtilities;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.model.Model;

import java.time.YearMonth;

import static com.jvm_bloggers.frontend.public_area.top_posts.SingleTopPostSummaryPage.buildPageParams;

public class TopPostsSummaryLink extends BookmarkablePageLink<SingleTopPostSummaryPage> {

    private static final String LABEL = "%s - podsumowanie najlepszych postów";

    public TopPostsSummaryLink(String id, YearMonth yearMonth) {
        super(id, SingleTopPostSummaryPage.class, buildPageParams(yearMonth));
        setBody(Model.of(String.format(LABEL, DateTimeUtilities.stringify(yearMonth))));
    }

}
