package com.jvm_bloggers.core.mailing

import com.jvm_bloggers.SpringContextAwareSpecification
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

@Subject(IssueNumberRetriever)
class IssueNumberRetrieverSpec extends SpringContextAwareSpecification {

    @Autowired
    IssueNumberRetriever issueNumberRetriever

    def "Should get next issue numbers starting from 8"() {
        when:
        long issueNumber1 = issueNumberRetriever.getNextIssueNumber()

        then:
        issueNumber1 == 8L

        when:
        long issueNumber2 = issueNumberRetriever.getNextIssueNumber()

        then:
        issueNumber2 == 9L
    }

}
