package com.jvm_bloggers.core.mailing

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.entities.metadata.Metadata
import com.jvm_bloggers.entities.metadata.MetadataKeys
import com.jvm_bloggers.entities.metadata.MetadataRepository
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue
import spock.lang.Specification
import spock.lang.Subject

@Subject(BlogSummaryMailGenerator)
class BlogSummaryMailGeneratorSpec extends Specification {

    private static final Long SAMPLE_ISSUE_NUMBER = 59L
    private static final String SAMPLE_ISSUE_LINK = "http://jvm-bloggers.com/issue/59"

    MetadataRepository metadataRepository = Stub(MetadataRepository)
    LinkGenerator linkGenerator = Stub(LinkGenerator)

    BlogSummaryMailGenerator blogSummaryMailGenerator = new BlogSummaryMailGenerator(metadataRepository, linkGenerator)

    def "should replace \$currentIssueLink\$, in greeting section, with link to the newest issue"() {
        given:
        metadataRepository.findByName(MetadataKeys.MAILING_GREETING) >>
            new Metadata(1, MetadataKeys.MAILING_GREETING, '$currentIssueLink$')
        linkGenerator.generateIssueLink(SAMPLE_ISSUE_NUMBER) >> SAMPLE_ISSUE_LINK

        when:
        String content = blogSummaryMailGenerator.prepareMailContent(
            NewsletterIssue.builder()
                .heading("")
                .varia("")
                .issueNumber(SAMPLE_ISSUE_NUMBER)
                .build())

        then:
        content == "$SAMPLE_ISSUE_LINK<br/><br/><br/>".toString()
    }
}
