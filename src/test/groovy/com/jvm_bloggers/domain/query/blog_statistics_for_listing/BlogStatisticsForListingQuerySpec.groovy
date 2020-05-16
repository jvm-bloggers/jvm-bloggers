package com.jvm_bloggers.domain.query.blog_statistics_for_listing

import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.projections.BlogStatisticsProjection
import io.vavr.collection.List
import io.vavr.control.Option
import spock.lang.Specification
import spock.lang.Subject

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL
import static java.time.LocalDateTime.now

@Subject(BlogStatisticsForListingQuery)
class BlogStatisticsForListingQuerySpec extends Specification {

    BlogRepository blogRepository = Stub(BlogRepository)
    BlogStatisticsForListingQuery blogStatisticsForListingQuery = new BlogStatisticsForListingQuery(
            blogRepository, new TestTimeProvider(now())
    )

    def "Should return blog statistics"() {
        given:
        BlogStatisticsProjection blogStatisticsProjection = [getBookmarkableId      : { 'bookmarkableId' },
                                                             getUrl: { 'url' },
                                                             getAuthor    : { 'author' },
                                                             getTwitter: { 'twitter' },
                                                             getFirstCount: { 10 },
                                                             getSecondCount: { 20 } ] as BlogStatisticsProjection
        blogRepository.findBlogStatistics(_, _, _, _) >> List.of(blogStatisticsProjection)

        when:
        List<BlogStatisticsForListing> result = blogStatisticsForListingQuery
                .findBlogPostStatistics(PERSONAL, 0, 1)

        then:
        result.length() == 1
        result[0].bookmarkableId == blogStatisticsProjection.bookmarkableId
        result[0].url == blogStatisticsProjection.url
        result[0].author == blogStatisticsProjection.author
        result[0].twitter == Option.of(blogStatisticsProjection.twitter)
        result[0].countFirstRange == blogStatisticsProjection.firstCount
        result[0].countSecondRange == blogStatisticsProjection.secondCount
    }

    def "Should count blogs by type"() {
        given:
        long currentCount = 10
        blogRepository.countByBlogType(PERSONAL) >> currentCount

        when:
        long countResult = blogStatisticsForListingQuery.countByBlogType(PERSONAL)

        then:
        countResult == currentCount
    }
}
