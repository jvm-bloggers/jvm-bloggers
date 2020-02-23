package com.jvm_bloggers.frontend.public_area.jvm_poland_slack;

import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;

import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("jvm-poland-slack")
public class JvmPolandSlackPage extends AbstractFrontendPage {

    @SpringBean
    public JvmPolandSlackPageBackingBean backingBean;

    public JvmPolandSlackPage() {
        add(new ExternalLink("slack_invitation_link", backingBean.getInvitationLink()));
    }

    @Override
    protected String getPageTitle() {
        return "JVM Poland Slack";
    }

}
