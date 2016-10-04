package com.jvm_bloggers.admin_panel.mailing;

import com.jvm_bloggers.core.mailing.domain.MailingAddress;
import com.jvm_bloggers.core.mailing.domain.MailingAddressRepository;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static com.jvm_bloggers.admin_panel.mailing.MailingAddressPage.DEFAULT_MODEL;

public class MailingAddressActionPanel extends Panel {
    public static final String DELETE_MAILING_ADDRESS_ID = "deleteMailingAddress";
    public static final String EDIT_MAILING_ADDRESS_ID = "editMailingAddress";

    @SpringBean
    private MailingAddressRepository mailingAddressRepository;

    public MailingAddressActionPanel(
            String id,
            Form<MailingAddress> mailingAddressForm,
            IModel<MailingAddress> model) {
        super(id);
        add(createDeleteButton(mailingAddressForm, model));
        add(createEditButton(mailingAddressForm, model));
    }

    private AjaxButton createEditButton(Form<MailingAddress> mailingAddressForm, IModel<MailingAddress> model) {
        AjaxButton editButton = new AjaxButton(EDIT_MAILING_ADDRESS_ID, mailingAddressForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                MailingAddress mailingAddress = model.getObject();
                mailingAddressForm.setModelObject(mailingAddress);
                target.add(form);
            }
        };
        editButton.setDefaultFormProcessing(false);
        return editButton;
    }

    private AjaxButton createDeleteButton(Form<MailingAddress> mailingAddressForm, IModel<MailingAddress> model) {
        AjaxButton deleteButton = new AjaxButton(DELETE_MAILING_ADDRESS_ID, mailingAddressForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                MailingAddress mailingAddress = model.getObject();
                mailingAddressRepository.delete(mailingAddress.getId());
                mailingAddressForm.setModelObject(DEFAULT_MODEL.get());
                getSession().success("Mailing address '" + mailingAddress.getAddress() + "' has been deleted.");
                target.add(form);
            }
        };
        deleteButton.setDefaultFormProcessing(false);
        return deleteButton;
    }
}
