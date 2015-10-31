package pl.tomaszdziurko.jvm_bloggers.mailing

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ContextConfiguration
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import spock.lang.Specification

@ContextConfiguration(classes = [JvmBloggersApplication], loader = SpringApplicationContextLoader)
class IssueNumberRetrieverSpec extends Specification {

    @Autowired
    IssueNumberRetriever issueNumberRetriever

    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", "password")
    }

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
