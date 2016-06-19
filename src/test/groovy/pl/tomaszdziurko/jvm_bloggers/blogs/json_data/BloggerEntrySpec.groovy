package pl.tomaszdziurko.jvm_bloggers.blogs.json_data

import com.fasterxml.jackson.databind.ObjectMapper
import spock.lang.Specification

class BloggerEntrySpec extends Specification {

    def "Should accept json from bloggers.json as input data during parse process"() {
        given:
            ObjectMapper mapper = new ObjectMapper()
            File json = new File("src/main/resources/blogs/bloggers.json")
        when:
            BloggersData bloggers = mapper.readValue(json, BloggersData.class);
        then:
            bloggers.bloggers.size() > 0
            BloggerEntry entry = bloggers.bloggers.get(0)
            entry.getJsonId() == 1L
            entry.name == "Tomasz Dziurko"
    }

}
