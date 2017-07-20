package com.jvm_bloggers.core.data_fetching.blogs.json_data

import com.google.common.base.CharMatcher
import com.jvm_bloggers.JvmBloggersApplication
import groovy.json.JsonSlurper
import org.springframework.beans.factory.annotation.Value
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.core.io.Resource
import org.springframework.test.context.ActiveProfiles
import org.springframework.test.context.ContextConfiguration
import spock.lang.Specification
import spock.lang.Subject

@ContextConfiguration
@SpringBootTest(classes = JvmBloggersApplication)
@ActiveProfiles("test")
class JsonBlogsDataFileValidationSpec extends Specification {

    @Value("classpath:blogs/bloggers.json")
    Resource bloggersJson

    @Value("classpath:blogs/companies.json")
    Resource companiesJson

    @Value("classpath:blogs/videos.json")
    Resource videosJson

    @Subject
    List<BloggerEntry> jsonEntries

    def setup() {
        JsonSlurper slurper = new JsonSlurper()
        jsonEntries = [bloggersJson, companiesJson, videosJson]
            .collect {json -> json.inputStream}
            .collect({stream ->
               BloggersData data = slurper.parse(stream)
               data.getBloggers()
            })
            .flatten()
    }

    def "should check that all bookmarkable IDs are lower-case, alphanumeric or hyphen"() {
        List<String> violatingIds = []

        when:
        jsonEntries.bookmarkableId.forEach({id ->
            CharMatcher lowercaseOrHyphen = (CharMatcher.inRange('a' as char, 'z' as char) as CharMatcher)
                .or(CharMatcher.inRange('0' as char, '9' as char) as CharMatcher)
                .or(CharMatcher.is('-' as char) as CharMatcher)
            if (!lowercaseOrHyphen.matchesAllOf(id)) {
                violatingIds.add(id)
            }
        })

        then:
        assert violatingIds.isEmpty() : "All bookmarkable IDs should be lower-cased, alphanumeric or hyphen. Found violations: $violatingIds"
    }

    def "should check that all bookmarkable IDs are unique"() {
        given:
        Set<String> uniqueIds = []
        List<String> duplicateIds = []

        when:
        jsonEntries.bookmarkableId.forEach({id ->
            boolean added = uniqueIds.add(id)
            if (!added) {
                duplicateIds.add(id)
            }
        })

        then:
        assert duplicateIds.isEmpty() : "Duplicate bookmarkable IDs found in json files: $duplicateIds"
    }

}

