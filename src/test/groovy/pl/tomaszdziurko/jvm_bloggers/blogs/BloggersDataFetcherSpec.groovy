package pl.tomaszdziurko.jvm_bloggers.blogs

import spock.lang.Specification

class BloggersDataFetcherSpec extends Specification {

    def "Should not throw exception when url is not valid"() {
        given:
            String urlString = "invalid"
            String urlString2 = "invalid"
            String urlString3 = "invalid"
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, urlString3, Stub(BloggersDataUpdater))
        when:
            fetcher.refreshData()
        then:
            notThrown Exception
    }

    def "Should update bloggers data from valid url"() {
        given:
            String urlString = getClass().getResource("test_bloggers.json").toExternalForm()
            String urlString2 = getClass().getResource("test_companies.json").toExternalForm()
            String urlString3 = getClass().getResource("test_videos.json").toExternalForm()
            BloggersDataUpdater bloggersDataUpdater = Mock(BloggersDataUpdater)
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, urlString3, bloggersDataUpdater)
        when:
            fetcher.refreshData()
        then:
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 2 })
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
    }

}
