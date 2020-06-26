package com.jvm_bloggers.frontend.public_area.top_posts;

import com.jvm_bloggers.domain.query.top_posts_summary.TopPostsSummaryBasicDetails;
import com.jvm_bloggers.frontend.common_components.TopPostsSummaryLink;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("/top-articles")
public class TopPostsPage extends AbstractFrontendPage {

    public static final String TOP_POST_SUMMARIES_ID = "topPostSummaries";
    public static final String SUMMARY_LINK_ID = "summaryLink";

    @SpringBean
    private TopPostsPageBackingBean backingBean;

    public TopPostsPage() {
        add(new ListView<>(
            TOP_POST_SUMMARIES_ID,
            backingBean.getAllSummaries().toJavaList()
            ) {
            @Override
            protected void populateItem(ListItem<TopPostsSummaryBasicDetails> item) {
                item.add(
                    new TopPostsSummaryLink(SUMMARY_LINK_ID, item.getModelObject().getYearMonth())
                );
            }
        });
    }

    @Override
    protected String getPageTitle() {
        return "Lista podsumowań z najlepszymi postami w każdym miesiącu";
    }

}
