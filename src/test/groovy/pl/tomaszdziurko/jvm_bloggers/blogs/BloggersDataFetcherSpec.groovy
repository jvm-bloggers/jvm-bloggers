package pl.tomaszdziurko.jvm_bloggers.blogs

import spock.lang.Specification

class BloggersDataFetcherSpec extends Specification {

    def "Should not throw exception during fetching when url is not valid"() {
        given:
            String urlString = "invalid"
            String urlString2 = "invalid"
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, Stub(BloggersDataUpdater))
        when:
            fetcher.refreshData()
        then:
            notThrown Exception
    }

    def "Should update bloggers data from valid url"(){
        given:
            String urlString = getClass().getResource("bloggers.json").toExternalForm()
            String urlString2 = getClass().getResource("companies.json").toExternalForm()
            BloggersDataUpdater bloggersDataUpdater = Mock(BloggersDataUpdater)
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2,  bloggersDataUpdater)
        when:
            fetcher.refreshData()
        then:
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 2 })
    }

}
