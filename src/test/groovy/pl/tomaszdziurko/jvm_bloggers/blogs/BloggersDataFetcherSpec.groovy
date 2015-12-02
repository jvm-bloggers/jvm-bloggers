package pl.tomaszdziurko.jvm_bloggers.blogs

import spock.lang.Specification

class BloggersDataFetcherSpec extends Specification {

    def "Should store empty optional when url is not valid"() {
        given:
            String urlString = "invalid"
            String urlString2 = "invalid"
        when:
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, Stub(BloggersDataUpdater))
        then:
            !fetcher.bloggersUrlOptional.isPresent()
            !fetcher.companiesUrlOptional.isPresent()
    }

    def "Should store valid URL with valid input"() {
        given:
            String urlString = "http://google.com"
            String urlString2 = "http://facebook.com"
        when:
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString, urlString2, Stub(BloggersDataUpdater))
        then:
            fetcher.bloggersUrlOptional.isPresent()
            fetcher.bloggersUrlOptional.get().host == "google.com"
            fetcher.companiesUrlOptional.isPresent()
            fetcher.companiesUrlOptional.get().host == "facebook.com"
    }

}
