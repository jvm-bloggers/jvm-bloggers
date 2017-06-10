package com.jvm_bloggers.frontend.public_area.contributors

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.core.github.ContributorsService
import com.jvm_bloggers.entities.github.Contributor
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import io.vavr.collection.List as VavrList

import static com.jvm_bloggers.frontend.public_area.contributors.ContributorsPage.FIRST_LEVEL_CONTRIBUTORS_LIST_ID

class ContributorsPageSpec extends MockSpringContextAwareSpecification {

    ContributorsService contributorsService = Stub(ContributorsService);
    RightFrontendSidebarBackingBean sidebarBackingBean = Stub(RightFrontendSidebarBackingBean)

    @Override
    protected void setupContext() {
        addBean(contributorsService)
        addBean(sidebarBackingBean)
    }

    def "should load page with contributors"() {
        given:
            VavrList<Contributor> contributors = VavrList.of(Stub(Contributor), Stub(Contributor))
        contributorsService.fetchContributors() >> contributors

        when:
        tester.startPage(ContributorsPage)

        then:
        tester.assertListView(FIRST_LEVEL_CONTRIBUTORS_LIST_ID, contributors.toJavaList())
    }

}
