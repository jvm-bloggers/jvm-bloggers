package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListing;
import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListingQuery;
import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;
import com.jvm_bloggers.frontend.common_components.infinite_scroll.InfinitePaginationPanel;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;
import com.jvm_bloggers.frontend.public_area.blogs.navigation.NavigationTabItem;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static com.jvm_bloggers.frontend.public_area.blogs.navigation.NavigationTabItem.Icon.MALE;
import static com.jvm_bloggers.frontend.public_area.blogs.navigation.NavigationTabItem.Icon.TELEVISION;
import static com.jvm_bloggers.frontend.public_area.blogs.navigation.NavigationTabItem.Icon.USERS;

public abstract class AbstractBlogsPage extends AbstractFrontendPage {

    static final String DATA_VIEW_ID = "pageable";
    static final String DATA_VIEW_WRAPPER_ID = "pageable-wrapper";
    static final String INFINITE_SCROLL_ID = "infinite-pager";
    static final String PERSONAL_TAB_ID = "personal";
    static final String COMPANY_TAB_ID = "company";
    static final String VIDEO_TAB_ID = "video";

    @SpringBean
    protected BlogRepository blogRepository;

    @SpringBean
    protected BlogPostRepository blogPostRepository;

    @SpringBean
    protected BlogStatisticsForListingQuery blogStatsForListingQuery;

    @SpringBean
    protected BlogWithStatisticsItemPopulator blogWithStatisticsItemPopulator;

    @SpringBean
    protected PaginationConfiguration paginationConfiguration;

    protected abstract BlogType getBlogType();

    protected abstract Class<? extends AbstractBlogsPage> getActiveClass();

    AbstractBlogsPage() {
        createNavigationItems();
        WebMarkupContainer pageableWrapper = new WebMarkupContainer(DATA_VIEW_WRAPPER_ID);
        pageableWrapper.setOutputMarkupId(true);
        add(pageableWrapper);
        DataView dataView = createBlogChannelList(DATA_VIEW_ID);
        pageableWrapper.add(dataView);
        pageableWrapper.add(new InfinitePaginationPanel(INFINITE_SCROLL_ID, dataView));
    }

    private void createNavigationItems() {
        add(new NavigationTabItem(PERSONAL_TAB_ID, "Personalne", MALE,
            PersonalBlogsPage.class, this.getActiveClass()));
        add(new NavigationTabItem(COMPANY_TAB_ID, "Firmowe", USERS,
            CompanyBlogsPage.class, this.getActiveClass()));
        add(new NavigationTabItem(VIDEO_TAB_ID, "Wideo", TELEVISION,
            VideoBlogsPage.class, this.getActiveClass()));
    }

    private DataView<BlogStatisticsForListing> createBlogChannelList(String id) {
        final DataView<BlogStatisticsForListing> dataView =
            new DataView<BlogStatisticsForListing>(id,
                new BlogsRequestHandler(blogStatsForListingQuery, blogRepository,
                    paginationConfiguration, getBlogType())) {
                @Override
                protected void populateItem(Item<BlogStatisticsForListing> item) {
                    blogWithStatisticsItemPopulator.populateItem(item);
                }
            };

        dataView.setItemsPerPage(paginationConfiguration.getDefaultPageSize());
        dataView.setOutputMarkupId(true);
        return dataView;
    }
}
