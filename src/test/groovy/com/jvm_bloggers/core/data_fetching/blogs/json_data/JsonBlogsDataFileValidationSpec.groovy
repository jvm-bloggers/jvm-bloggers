package com.jvm_bloggers.core.data_fetching.blogs.json_data

import com.google.common.base.CharMatcher
import groovy.json.JsonSlurper
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

class JsonBlogsDataFileValidationSpec extends Specification {

    @Subject
    @Shared
    List<BloggerEntry> jsonEntries

    private static final int TWITTER_USER_TAG_MAXIMUM_LENGTH = 16

    private static final CharMatcher alphanumericOrUnderscore = (CharMatcher.inRange('a' as char, 'z' as char) as CharMatcher)
            .or(CharMatcher.inRange('A' as char, 'Z' as char) as CharMatcher)
            .or(CharMatcher.inRange('0' as char, '9' as char) as CharMatcher)
            .or(CharMatcher.is('_' as char) as CharMatcher)

    private static final CharMatcher lowercaseOrHyphen = (CharMatcher.inRange('a' as char, 'z' as char) as CharMatcher)
            .or(CharMatcher.inRange('0' as char, '9' as char) as CharMatcher)
            .or(CharMatcher.is('-' as char) as CharMatcher)

    def setupSpec() {
        File bloggersJson = new File('src/main/resources/blogs/bloggers.json')
        File companiesJson = new File('src/main/resources/blogs/companies.json')
        File videosJson = new File('src/main/resources/blogs/videos.json')

        JsonSlurper slurper = new JsonSlurper()
        jsonEntries = [bloggersJson, companiesJson, videosJson]
            .collect({stream ->
               BloggersData data = slurper.parse(stream)
               data.getBloggers()
            })
            .flatten()
    }

    @Unroll
    def "should check that bookmarkable ID is lower-case, alphanumeric or hyphen"() {
        expect:
        lowercaseOrHyphen.matchesAllOf(id)

        where:
        jsonHandle << jsonEntries
        id = jsonHandle.bookmarkableId
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

    @Unroll
    def "should check that twitter user tag starts with @"() {
        expect:
        tag.startsWith("@")

        where:
        tag << allTwitterUsersTags()
    }


    @Unroll
    def "should check that twitter user tag is less than 16 characters"() {
        expect:
        tag.length() <= TWITTER_USER_TAG_MAXIMUM_LENGTH

        where:
        tag << allTwitterUsersTags()
    }

    @Unroll
    def "should check that twitter username is alphanumeric or underscore"() {
        expect:
        alphanumericOrUnderscore.matchesAllOf(username)

        where:
        tag << allTwitterUsersTags()
        username = tag.substring(1)
    }

    private List<String> allTwitterUsersTags() {
        jsonEntries.findAll{ it.twitter != null }
                .twitter
    }
}
