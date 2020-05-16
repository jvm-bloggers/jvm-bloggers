package com.jvm_bloggers.frontend.public_area.jvm_poland_slack

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import org.apache.wicket.markup.html.link.ExternalLink

class JvmPolandSlackPageSpec extends MockSpringContextAwareSpecification {

    RightFrontendSidebarBackingBean sidebarBean = Stub(RightFrontendSidebarBackingBean)
    JvmPolandSlackPageBackingBean backingBean = Stub(JvmPolandSlackPageBackingBean)

    @Override
    protected void setupContext() {
        addBean(sidebarBean)
        addBean(backingBean)
    }

    def "Should contain invitation link"() {
        when:
        tester.startPage(JvmPolandSlackPage)

        then:
        tester.assertComponent("slack_invitation_link", ExternalLink)
    }

    def "Should contain link from properties"() {
        given:
        backingBean.getInvitationLink() >> defaultInvitationLink

        when:
        tester.startPage(JvmPolandSlackPage)

        then:
        tester.assertContains(backingBean.getInvitationLink())

        where:
        defaultInvitationLink << ["http://example-invititation-link"]
    }
}
