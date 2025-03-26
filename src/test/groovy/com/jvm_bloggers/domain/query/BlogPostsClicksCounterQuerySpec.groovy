package com.jvm_bloggers.domain.query

import com.jvm_bloggers.entities.click.ClickRepository
import com.jvm_bloggers.entities.click.PostClicksCountByIpAddress
import com.jvm_bloggers.entities.click.PostClicksCountByUserAgent
import io.vavr.collection.List
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

class BlogPostsClicksCounterQuerySpec extends Specification {

    ClickRepository clickRepository = Stub()

    @Subject
    BlogPostsClicksCounterQuery botClicksDetectorQuery = new BlogPostsClicksCounterQuery(clickRepository)


    def "Should return top blog post clicks aggregated by ip address"() {
        given:
        PostClicksCountByIpAddress postClicksCountByIpAddress = new PostClicksCountByIpAddress(
                'Title', 'Author', '127.0.0.1', 10)
        clickRepository.topPostsClicksCountByIpAddress(_, _, _) >> List.of(postClicksCountByIpAddress)

        when:
        List<PostClicksCountByIpAddress> postClicksByIpAddress = botClicksDetectorQuery
                .topPostsClicksCountByIpAddress(LocalDateTime.now(), LocalDateTime.now(), 10)

        then:
        postClicksByIpAddress.size() == 1
        postClicksByIpAddress.get(0) == postClicksCountByIpAddress
    }

    def "Should return top blog post clicks aggregated by user agent"() {
        given:
        PostClicksCountByUserAgent postClicksCountByUserAgent = new PostClicksCountByUserAgent(
                'Title', 'Author', 'User Agent', 11)
        clickRepository.topPostsClicksCountByUserAgent(_, _, _) >> List.of(postClicksCountByUserAgent)

        when:
        List<PostClicksCountByUserAgent> postClicksByUserAgent = botClicksDetectorQuery
                .topPostsClicksCountByUserAgent(LocalDateTime.now(), LocalDateTime.now(), 10)

        then:
        postClicksByUserAgent.size() == 1
        postClicksByUserAgent.get(0) == postClicksCountByUserAgent
    }
}