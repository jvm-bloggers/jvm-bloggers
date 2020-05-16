package com.jvm_bloggers.core.data_fetching.blogs

import com.fasterxml.jackson.databind.ObjectMapper
import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.entities.metadata.MetadataRepository
import com.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

@Subject(BloggersDataFetcher)
class BloggersDataFetcherSpec extends Specification {

    private static final ObjectMapper MAPPER = new ObjectMapper()

    def "Should not throw exception when url is not valid"() {
        given:
        LocalDateTime NOW = LocalDateTime.now()
        NowProvider nowProvider = new TestTimeProvider(NOW)
        String urlString = 'invalid', urlString2 = 'invalid', urlString3 = 'invalid', urlString4 = 'invalid'
        BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, urlString3, urlString4, Stub(BloggersDataUpdater), MAPPER, Stub(MetadataRepository), nowProvider)

        when:
        fetcher.refreshData()

        then:
        notThrown Exception
    }

    def "Should update bloggers data from valid url"() {
        given:
        LocalDateTime NOW = LocalDateTime.now()
        NowProvider nowProvider = new TestTimeProvider(NOW)
        String urlString = getClass().getResource('test_bloggers.json').toExternalForm()
        String urlString2 = getClass().getResource('test_companies.json').toExternalForm()
        String urlString3 = getClass().getResource('test_presentations.json').toExternalForm()
        String urlString4 = getClass().getResource('test_podcasts.json').toExternalForm()
        BloggersDataUpdater bloggersDataUpdater = Mock(BloggersDataUpdater)
        BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, urlString3, urlString4, bloggersDataUpdater, MAPPER, Stub(MetadataRepository), nowProvider)

        when:
        fetcher.refreshData()

        then:
        1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
        1 * bloggersDataUpdater.updateData({ it.bloggers.size == 2 })
        1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
        1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
    }

}
