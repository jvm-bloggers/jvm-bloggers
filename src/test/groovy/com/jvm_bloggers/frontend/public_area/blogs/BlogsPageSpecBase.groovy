package com.jvm_bloggers.frontend.public_area.blogs

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListing
import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListingQuery
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.projections.BlogStatisticsProjection
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import com.jvm_bloggers.utils.NowProvider

import java.time.LocalDateTime

class BlogsPageSpecBase extends MockSpringContextAwareSpecification {

    BlogRepository blogRepository = Mock()
    BlogPostRepository blogPostRepository = Mock()
    BlogStatisticsForListingQuery blogStatisticsForListingQuery = Mock()
    BlogWithStatisticsItemPopulator blogWithStatisticsItemPopulator = new BlogWithStatisticsItemPopulator()
    NowProvider nowProvider = Stub() { now() >> LocalDateTime.now() }
    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(2)
    RightFrontendSidebarBackingBean sidebarBackingBean = Stub()

    @Override
    protected void setupContext() {
        addBean(blogRepository)
        addBean(blogPostRepository)
        addBean(blogStatisticsForListingQuery)
        addBean(blogWithStatisticsItemPopulator)
        addBean(nowProvider)
        addBean(paginationConfiguration)
        addBean(sidebarBackingBean)
    }

    protected BlogStatisticsForListing createBlogStatisticsForListing(long id, int firstCount, int secondCount) {
        BlogStatisticsForListing.fromBlogPostStatisticProjection([getId         : { id }, getUrl: { "url" + id },
                                                                  getAuthor     : { "author" + id },
                                                                  getTwitter    : { "twitter" + id },
                                                                  getFirstCount : { firstCount },
                                                                  getSecondCount: { secondCount }
        ] as BlogStatisticsProjection)
    }
}
