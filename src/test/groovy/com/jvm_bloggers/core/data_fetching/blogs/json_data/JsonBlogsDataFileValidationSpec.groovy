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

    private static final int AT_PLUS_TWITTER_HANDLE_MAX_LENGTH = 16

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
        File presentationsJson = new File('src/main/resources/blogs/presentations.json')
        File podcastsJson = new File('src/main/resources/blogs/podcasts.json')

        JsonSlurper slurper = new JsonSlurper()
        jsonEntries = [bloggersJson, companiesJson, presentationsJson, podcastsJson]
            .collect({stream ->
               BloggersData data = slurper.parse(stream)
               data.getBloggers()
            })
            .flatten()
    }

    @Unroll
    def "should check that bookmarkable ID #id is lower-case, alphanumeric or hyphen"() {
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
    def "should check that @ + twitter handle #atPlusTwitterHandle starts with @"() {
        expect:
        atPlusTwitterHandle.startsWith("@")

        where:
        atPlusTwitterHandle << allAtPlusTwitterUserNames()
    }

    @Unroll
    def "should check that @ + twitter handle #atPlusTwitterHandle is less than 16 characters"() {
        expect:
        atPlusTwitterHandle.length() <= AT_PLUS_TWITTER_HANDLE_MAX_LENGTH

        where:
        atPlusTwitterHandle << allAtPlusTwitterUserNames()
    }

    @Unroll
    def "should check that twitter username #username is alphanumeric or underscore"() {
        expect:
        alphanumericOrUnderscore.matchesAllOf(username)

        where:
        atPlusTwitterHandle << allAtPlusTwitterUserNames()
        username = atPlusTwitterHandle.substring(1)
    }

    private List<String> allAtPlusTwitterUserNames() {
        jsonEntries.findAll{ it.twitter != null }
                .twitter
    }
}
