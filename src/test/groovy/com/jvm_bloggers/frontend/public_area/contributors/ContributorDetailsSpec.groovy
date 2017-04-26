package com.jvm_bloggers.frontend.public_area.contributors

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.github.Contributor
import org.apache.wicket.markup.html.link.AbstractLink
import org.apache.wicket.model.IModel
import org.apache.wicket.model.Model
import spock.lang.Subject

import static org.junit.Assert.assertEquals

class ContributorDetailsSpec extends MockSpringContextAwareSpecification {

    public static final int CONTRIBUTIONS = 666
    public static final String LOGIN = "LOGIN"
    public static final String AVATAR_URL = "sth.png"
    public static final String PROFILE_URL = "https://github.com/sth"

    @Override
    protected void setupContext() {

    }

    def "Should have label with number of contributions"() {
        given:
        Contributor contributor = new Contributor()
        contributor.setContributions(CONTRIBUTIONS)

        IModel<Contributor> model = Model.of(contributor)

        @Subject
        ContributorDetails testObj = new ContributorDetails("contributorDetails", model)

        when:
        tester.startComponentInPage(testObj)

        then:
        tester.assertLabel("contributorDetails:contributions", "666")
    }

    def "Should have login linking to profile page"() {
        given:
        Contributor contributor = new Contributor()
        contributor.setLogin(LOGIN)
        contributor.setProfilePage(PROFILE_URL)

        IModel<Contributor> model = Model.of(contributor)

        @Subject
        ContributorDetails testObj = new ContributorDetails("contributorDetails", model)

        when:
        tester.startComponentInPage(testObj)

        then:
        AbstractLink link = tester.getComponentFromLastRenderedPage("contributorDetails:link")
        assertEquals(PROFILE_URL, link.getDefaultModelObject())
        assertEquals(LOGIN, link.getBody().getObject())
    }

    def "Should have avatar linking to profile page"() {
        given:
        Contributor contributor = new Contributor()
        contributor.setAvatarUrl(AVATAR_URL)
        contributor.setLogin(LOGIN)
        contributor.setProfilePage(PROFILE_URL)

        IModel<Contributor> model = Model.of(contributor)

        @Subject
        ContributorDetails testObj = new ContributorDetails("contributorDetails", model)

        when:
        tester.startComponentInPage(testObj)

        then:
        tester.assertModelValue("contributorDetails:avatarLink", PROFILE_URL)
        tester.assertModelValue("contributorDetails:avatarLink:avatar", AVATAR_URL)
    }
}
