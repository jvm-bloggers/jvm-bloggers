package com.jvm_bloggers.frontend.public_area.blogs.timeline_panel;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;
import com.jvm_bloggers.frontend.admin_area.blogs.BlogPostsPageRequestHandler;
import com.jvm_bloggers.frontend.common_components.infinite_scroll.InfinitePaginationPanel;
import com.jvm_bloggers.utils.NowProvider;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.time.YearMonth;

public class BlogPostsTimelinePanel extends Panel {

    static final String BLOG_POST_DETAILS_ID = "blogPostDetails";
    static final String DATA_VIEW_ID = "pageable";
    static final String DATA_VIEW_WRAPPER_ID = "pageable-wrapper";
    static final String INFINITE_SCROLL_ID = "infinite-pager";
    static final String YEAR_MONTH_SEPARATOR_ID = "monthYear";
    static final String LEFT_CSS_CLASS = "col-sm-12 col-md-5 left-part";
    static final String RIGHT_OFFSET_5_CSS_CLASS =
        "col-sm-12 col-md-5 col-md-offset-2 right-part offset-top";
    static final String RIGHT_OFFSET_7_CSS_CLASS =
        "col-sm-12 col-md-5 col-md-offset-7 right-part offset-top";

    @SpringBean
    PaginationConfiguration paginationConfiguration;

    @SpringBean
    NowProvider nowProvider;

    private final BlogPostsPageRequestHandler requestHandler;

    private final DataView<BlogPost> dataView;

    private int iterationCounter = 0;

    private YearMonth currentYearMonth;

    public BlogPostsTimelinePanel(String id, BlogPostsPageRequestHandler requestHandler) {
        super(id);
        Injector.get().inject(this);
        this.requestHandler = requestHandler;
        currentYearMonth = YearMonth.from(nowProvider.now());
        dataView = createBlogPostDataView();
        WebMarkupContainer pageableWrapper = new WebMarkupContainer(DATA_VIEW_WRAPPER_ID);
        add(pageableWrapper);
        pageableWrapper.add(dataView);
        pageableWrapper.add(new InfinitePaginationPanel(INFINITE_SCROLL_ID,
            pageableWrapper, dataView));
    }

    private DataView<BlogPost> createBlogPostDataView() {
        final DataView<BlogPost> dataView =
            new DataView<BlogPost>(DATA_VIEW_ID, requestHandler) {
                @Override
                protected void populateItem(Item<BlogPost> item) {
                    BlogPost post = item.getModelObject();
                    YearMonth yearMonth = YearMonth.from(post.getPublishedDate());
                    YearMonthSeparatorPanel yearMonthSeparatorPanel = new YearMonthSeparatorPanel(
                        YEAR_MONTH_SEPARATOR_ID, yearMonth);
                    yearMonthSeparatorPanel.setVisible(false);
                    if (iterationCounter == 0 || currentYearMonth.isAfter(yearMonth)) {
                        yearMonthSeparatorPanel.setVisible(true);
                        currentYearMonth = yearMonth;
                    }
                    item.add(yearMonthSeparatorPanel);
                    item.add(new BlogPostDetailsPanel(BLOG_POST_DETAILS_ID,
                        getOrientationCssClass(iterationCounter,
                            yearMonthSeparatorPanel.isVisible()), post));
                    iterationCounter++;
                }
            };

        dataView.setItemsPerPage(paginationConfiguration.getDefaultPageSize());
        dataView.setOutputMarkupId(true);
        return dataView;
    }

    private String getOrientationCssClass(int counter, boolean separatorVisible) {
        if (counter % 2 == 0) {
            return LEFT_CSS_CLASS;
        }
        if (counter % dataView.getItemsPerPage() == 0) {
            return RIGHT_OFFSET_7_CSS_CLASS;
        }
        return separatorVisible ? RIGHT_OFFSET_7_CSS_CLASS : RIGHT_OFFSET_5_CSS_CLASS;
    }
}
