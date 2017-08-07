package com.jvm_bloggers.frontend.admin_area
import com.jvm_bloggers.MockSpringContextAwareSpecification
import org.apache.wicket.util.tester.FormTester

class AdminSocialChannelsPageSpec extends MockSpringContextAwareSpecification {
    static final String SAMPLE_VALID_LINK = "http://link.com"
    static final String SAMPLE_VALID_MESSAGE = "valid message"

    AdminSocialChannelsPageBackingBean backingBean = Mock(AdminSocialChannelsPageBackingBean)

    @Override
    protected void setupContext() {
        addBean(backingBean)
        addBean(Stub(PaginationConfiguration))
    }

    def "Should send a CreateNewFacebookPost command"() {
        given:
        currentUserIsAdmin()
        tester.startPage(AdminSocialChannelsPage)

        when:
        FormTester formTester = tester.newFormTester(AdminSocialChannelsPage.FACEBOOK_POST_FORM_ID, false)
        formTester.setValue(AdminSocialChannelsPage.LINK_INPUT_ID, SAMPLE_VALID_LINK)
        formTester.setValue(AdminSocialChannelsPage.MESSAGE_INPUT_ID, SAMPLE_VALID_MESSAGE)
        formTester.submit(AdminSocialChannelsPage.SAVE_BUTTON_ID)

        then:
        1 * backingBean.createNewFacebookPost(_,_)
    }

    def "Should not send createNewFacebookPost command when link is not valid"(){
        given:
        currentUserIsAdmin()
        tester.startPage(AdminSocialChannelsPage)

        when:
        FormTester formTester = tester.newFormTester(AdminSocialChannelsPage.FACEBOOK_POST_FORM_ID)
        formTester.setValue(AdminSocialChannelsPage.LINK_INPUT_ID,"")
        formTester.setValue(AdminSocialChannelsPage.MESSAGE_INPUT_ID,SAMPLE_VALID_MESSAGE)
        formTester.submit(AdminSocialChannelsPage.SAVE_BUTTON_ID)

        then:
        0 * backingBean.createNewFacebookPost(_)
    }
}
