package pl.tomaszdziurko.jvm_bloggers.people

import spock.lang.Specification

class BloggersDataFetcherSpec extends Specification {

    def "Should store empty optional when url is not valid"() {
        given:
            String urlString = "invalid"
        when:
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString)
        then:
            !fetcher.urlOptional.isPresent()
    }

    def "Should store valid URL with valid input"() {
        given:
            String urlString = "http://google.com"
        when:
            BloggersDataFetcher fetcher = new BloggersDataFetcher(urlString)
        then:
            fetcher.urlOptional.isPresent()
            fetcher.urlOptional.get().host == "google.com"
    }


}
