package com.jvm_bloggers.domain.query.blog_statistics_for_listing

import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.projections.BlogStatisticsProjection
import javaslang.collection.List
import javaslang.control.Option
import org.springframework.data.domain.PageRequest
import spock.lang.Specification
import spock.lang.Subject

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class BlogStatisticsForListingQuerySpec extends Specification {

    BlogRepository blogRepository = Mock(BlogRepository)

    @Subject
    BlogStatisticsForListingQuery blogStatisticsForListingQuery = new BlogStatisticsForListingQuery(blogRepository)

    def "Should return blog statistics"() {
        given:
        BlogStatisticsProjection blogStatisticsProjection = [getId        : { 1L }, getUrl: { 'url' },
                                                             getAuthor    : { 'author' },
                                                             getTwitter: { 'twitter' },
                                                             getFirstCount: { 10 },
                                                             getSecondCount: { 20 } ] as BlogStatisticsProjection
        blogRepository.findBlogStatistics(_, _, _, _) >> List.of(blogStatisticsProjection)

        when:
        List<BlogStatisticsForListing> result = blogStatisticsForListingQuery
                .findBlogPostStatistics(PERSONAL, new PageRequest(0, 1))

        then:
        result.length() == 1
        result[0].id == blogStatisticsProjection.id
        result[0].url == blogStatisticsProjection.url
        result[0].author == blogStatisticsProjection.author
        result[0].twitter == Option.of(blogStatisticsProjection.twitter)
        result[0].countFirstRange == blogStatisticsProjection.firstCount
        result[0].countSecondRange == blogStatisticsProjection.secondCount
    }
}
