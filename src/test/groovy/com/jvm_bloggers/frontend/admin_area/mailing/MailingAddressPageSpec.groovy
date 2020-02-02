package com.jvm_bloggers.frontend.admin_area.mailing

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.mailing_address.MailingAddress
import com.jvm_bloggers.entities.mailing_address.MailingAddressRepository
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import org.apache.wicket.model.Model
import org.apache.wicket.util.tester.FormTester

import static com.jvm_bloggers.frontend.admin_area.mailing.MailingAddressActionPanel.DELETE_MAILING_ADDRESS_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.MailingAddressActionPanel.EDIT_MAILING_ADDRESS_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.MailingAddressPage.ACTION_PANEL_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.MailingAddressPage.PAGEABLE_LIST_ID

class MailingAddressPageSpec extends MockSpringContextAwareSpecification {

    static final String SAMPLE_VALID_ADDRESS = "joe.doe@mail.com"
    static final Long SAMPLE_ID = 1

    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(15);
    MailingAddressPageRequestHandler requestHandler = Mock(MailingAddressPageRequestHandler)
    MailingAddressRepository mailingAddressRepository = Mock(MailingAddressRepository)

    void setupContext() {
        addBean(paginationConfiguration)
        addBean(requestHandler)
        addBean(mailingAddressRepository)
    }

    def "Should create new MailingAddress"() {
        when:
        tester.startPage(MailingAddressPage.class)
        FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
        formTester.setValue(MailingAddressPage.ADDRESS_INPUT_ID, SAMPLE_VALID_ADDRESS)
        formTester.submit(MailingAddressPage.SAVE_BUTTON_ID)

        then:
        1 * requestHandler.save({ it.address == SAMPLE_VALID_ADDRESS && it.id == null })
    }

    def "Should reject not valid MailingAddress save request"() {
        when:
        tester.startPage(MailingAddressPage.class)
        FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
        formTester.setValue(MailingAddressPage.ADDRESS_INPUT_ID, "")
        formTester.submit(MailingAddressPage.SAVE_BUTTON_ID)

        then:
        0 * requestHandler.save(_)
    }

    def "Should not save MailingAddress when address not unique"() {
        given:
        mailingAddressRepository.addressExistsIgnoringId(_, _) >> true

        when:
        tester.startPage(MailingAddressPage.class)
        FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
        formTester.setValue(MailingAddressPage.ADDRESS_INPUT_ID, SAMPLE_VALID_ADDRESS)
        formTester.submit(MailingAddressPage.SAVE_BUTTON_ID)

        then:
        0 * requestHandler.save(_)
        tester.assertErrorMessages("Address 'joe.doe@mail.com' already exists.")
    }

    def "Should delete MailingAddress"() {
        given:
        MailingAddress mailingAddress = newSampleMailingAddress()
        requestHandler.iterator(_, _) >> [mailingAddress].iterator()
        requestHandler.size() >> 1
        requestHandler.model(_) >> Model.of(mailingAddress)

        when:
        tester.startPage(MailingAddressPage.class)
        FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
        formTester.submit([PAGEABLE_LIST_ID, 1, ACTION_PANEL_ID, DELETE_MAILING_ADDRESS_ID].join(":"))

        then:
        1 * requestHandler.delete(mailingAddress.id)
    }

    def "Should update selected MailingAddress"() {
        given:
        MailingAddress mailingAddress = newSampleMailingAddress()
        requestHandler.iterator(_, _) >> [mailingAddress].iterator()
        requestHandler.size() >> 1
        requestHandler.model(_) >> Model.of(mailingAddress)

        when:
        tester.startPage(MailingAddressPage.class)
        FormTester formTester = tester.newFormTester(MailingAddressPage.MAILING_ADDRESS_FORM_ID)
        formTester.submit(buttonPath(EDIT_MAILING_ADDRESS_ID))
        formTester.submit(MailingAddressPage.SAVE_BUTTON_ID)

        then:
        1 * requestHandler.save(mailingAddress)
    }

    private def MailingAddress newSampleMailingAddress() {
        MailingAddress mailingAddress = new MailingAddress(SAMPLE_VALID_ADDRESS)
        mailingAddress.id = SAMPLE_ID
        return mailingAddress
    }

    private def String buttonPath(String buttonId) {
        [PAGEABLE_LIST_ID, 1, ACTION_PANEL_ID, buttonId].join(":")
    }
}
