package com.jvm_bloggers.frontend.public_area.contributors

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.core.github.ContributorsService
import com.jvm_bloggers.entities.github.Contributor
import com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssueDtoService

class ContributorsPageSpec extends MockSpringContextAwareSpecification {

    ContributorsService contributorsService = Stub(ContributorsService);
    NewsletterIssueDtoService newsletterIssueService = Stub(NewsletterIssueDtoService)

    @Override
    protected void setupContext() {
        addBean(contributorsService)
        addBean(newsletterIssueService)
    }

    def "Name"() {
        given:
            List<Contributor> contributors = [Stub(Contributor), Stub(Contributor)]
            contributorsService.fetchContributors() >> contributors

        when:
            tester.startPage(ContributorsPage)

        then:
            tester.assertListView("contributorsList", contributors)
    }
}
