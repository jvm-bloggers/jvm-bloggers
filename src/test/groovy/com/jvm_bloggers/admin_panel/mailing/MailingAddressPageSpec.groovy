package com.jvm_bloggers.admin_panel.mailing

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.admin_panel.PaginationConfiguration
import com.jvm_bloggers.core.mailing.domain.MailingAddress
import com.jvm_bloggers.core.mailing.domain.MailingAddressRepository
import org.apache.wicket.util.tester.FormTester
import org.springframework.data.domain.PageImpl

import static com.jvm_bloggers.admin_panel.mailing.MailingAddressActionPanel.DELETE_MAILING_ADDRESS_ID
import static com.jvm_bloggers.admin_panel.mailing.MailingAddressActionPanel.EDIT_MAILING_ADDRESS_ID
import static com.jvm_bloggers.admin_panel.mailing.MailingAddressPage.ACTION_PANEL_ID
import static com.jvm_bloggers.admin_panel.mailing.MailingAddressPage.PAGEABLE_LIST_ID

class MailingAddressPageSpec extends MockSpringContextAwareSpecification {

    static final String SAMPLE_VALID_ADDRESS = "joe.doe@mail.com"
    static final Long SAMPLE_ID = 1

    MailingAddressRepository mailingAddressRepository = Mock(MailingAddressRepository)
    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(15);

    def void setupContext() {
        addBean(mailingAddressRepository)
        addBean(paginationConfiguration)
        addBean(new MailingAddressPageRequestHandler(paginationConfiguration, mailingAddressRepository))
    }

    def "Should create new MailingAddress" () {
        given:
            mailingAddressRepository.addressExistsIgnoringId(_, _) >> false
        when:
            tester.startPage(MailingAddressPage.class)
            FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
            formTester.setValue(MailingAddressPage.ADDRESS_INPUT_ID, SAMPLE_VALID_ADDRESS)
            formTester.submit(MailingAddressPage.SAVE_BUTTON_ID)
        then:
            1 * mailingAddressRepository.save( {it.address == SAMPLE_VALID_ADDRESS && it.id == null})
    }

    def "Should reject not valid MailingAddress save request"() {
        given:
            mailingAddressRepository.addressExistsIgnoringId(_, _) >> false
        when:
            tester.startPage(MailingAddressPage.class)
            FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
            formTester.setValue(MailingAddressPage.ADDRESS_INPUT_ID, "")
            formTester.submit(MailingAddressPage.SAVE_BUTTON_ID)
        then:
            0 * mailingAddressRepository.save(_)
    }

    def "Should not save MailingAddress when address not unique" () {
        given:
            mailingAddressRepository.addressExistsIgnoringId(_, _) >> true
        when:
            tester.startPage(MailingAddressPage.class)
            FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
            formTester.setValue(MailingAddressPage.ADDRESS_INPUT_ID, SAMPLE_VALID_ADDRESS)
            formTester.submit(MailingAddressPage.SAVE_BUTTON_ID)
        then:
            0 * mailingAddressRepository.save(_)
            tester.assertErrorMessages("Address 'joe.doe@mail.com' already exists.")
    }

    def "Should delete MailingAddress"(){
        given:
            MailingAddress mailingAddress = newSampleMailingAddress()
            mailingAddressRepository.findAllByOrderByAddressAsc(_) >> new PageImpl([mailingAddress])
            mailingAddressRepository.count() >> 1
            mailingAddressRepository.findOne(mailingAddress.id) >> mailingAddress
        when:
            tester.startPage(MailingAddressPage.class)
            FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
            formTester.submit([PAGEABLE_LIST_ID, 1, ACTION_PANEL_ID, DELETE_MAILING_ADDRESS_ID].join(":"))
        then:
            1 * mailingAddressRepository.delete(mailingAddress.id)
    }

    def "Should update selected MailingAddress"(){
        given:
            MailingAddress mailingAddress = newSampleMailingAddress()
            mailingAddressRepository.findAllByOrderByAddressAsc(_) >> new PageImpl([mailingAddress])
            mailingAddressRepository.count() >> 1
            mailingAddressRepository.findOne(mailingAddress.id) >> mailingAddress
            mailingAddressRepository.addressExistsIgnoringId(_, _) >> false
        when:
            tester.startPage(MailingAddressPage.class)
            FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
            formTester.submit(buttonPath(EDIT_MAILING_ADDRESS_ID))
            formTester.submit(MailingAddressPage.SAVE_BUTTON_ID)
        then:
            1 * mailingAddressRepository.save(mailingAddress)
    }

    private def MailingAddress newSampleMailingAddress(){
        MailingAddress mailingAddress = new MailingAddress(SAMPLE_VALID_ADDRESS)
        mailingAddress.id = SAMPLE_ID
        return mailingAddress
    }

    private def String buttonPath(String buttonId){
        [PAGEABLE_LIST_ID, 1, ACTION_PANEL_ID, buttonId].join(":")
    }
}
