package com.jvm_bloggers.core.social.fb

import spock.lang.Specification

class FacebookMessageGeneratorSpec extends Specification {

    def "Should generate a Facebook message with an issue link"() {
        given:
            FacebookMessageGenerator generator = new FacebookMessageGenerator()
            String issueLink = "http://jvm-bloggers.com/issue/1"

        when:
            String facebookMessage = generator.generateFacebookMessage(issueLink)

        then:
            facebookMessage.contains(issueLink)
    }

    def "Should generate a Facebook message with an issue heading"() {
        given:
            FacebookMessageGenerator generator = new FacebookMessageGenerator()
            String issueLink = "http://jvm-bloggers.com/issue/1"

        when:
            String facebookMessage = generator.generateFacebookMessage(issueLink)

        then:
            facebookMessage.contains(issueLink)
    }

}