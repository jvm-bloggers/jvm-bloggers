package com.jvm_bloggers.core.data_fetching.blogs

import com.fasterxml.jackson.databind.ObjectMapper
import com.jvm_bloggers.TestNowProvider
import com.jvm_bloggers.entities.metadata.MetadataRepository
import com.jvm_bloggers.utils.NowProvider
import spock.lang.Specification

import java.time.LocalDateTime

class BloggersDataFetcherSpec extends Specification {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    def "Should not throw exception when url is not valid"() {
        given:
            LocalDateTime NOW = LocalDateTime.now()
            NowProvider nowProvider = new TestNowProvider(NOW)
            String urlString = "invalid"
            String urlString2 = "invalid"
            String urlString3 = "invalid"
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, urlString3, Stub(BloggersDataUpdater), MAPPER, Stub(MetadataRepository), nowProvider)
        when:
            fetcher.refreshData()
        then:
            notThrown Exception
    }

    def "Should update bloggers data from valid url"() {
        given:
            LocalDateTime NOW = LocalDateTime.now()
            NowProvider nowProvider = new TestNowProvider(NOW)
            String urlString = getClass().getResource("test_bloggers.json").toExternalForm()
            String urlString2 = getClass().getResource("test_companies.json").toExternalForm()
            String urlString3 = getClass().getResource("test_videos.json").toExternalForm()
            BloggersDataUpdater bloggersDataUpdater = Mock(BloggersDataUpdater)
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, urlString3, bloggersDataUpdater, MAPPER, Stub(MetadataRepository), nowProvider)
        when:
            fetcher.refreshData()
        then:
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 2 })
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
    }

}
