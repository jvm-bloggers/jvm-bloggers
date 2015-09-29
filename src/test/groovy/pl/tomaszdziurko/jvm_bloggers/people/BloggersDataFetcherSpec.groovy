package pl.tomaszdziurko.jvm_bloggers.people

import spock.lang.Specification
import org.springframework.core.io.ClassPathResource;

class BloggersDataFetcherSpec extends Specification {

    def "Should store empty optional when path is not valid"() {
        given:
            String path = "invalid"
        when:
            BloggersDataFetcher fetcher = new BloggersDataFetcher(new ClassPathResource(path), Stub(BloggersDataUpdater))
        then:
            !fetcher.resourceOptional.isPresent()
    }

    def "Should store valid File with valid input"() {
        given:
            String path = "bloggers.json"
        when:
            BloggersDataFetcher fetcher = new BloggersDataFetcher(new ClassPathResource(path), Stub(BloggersDataUpdater))
        then:
            fetcher.resourceOptional.isPresent()
            fetcher.resourceOptional.get().exists()
    }

    def "Should properly load entry from bloggers.json"() {


    }


}
