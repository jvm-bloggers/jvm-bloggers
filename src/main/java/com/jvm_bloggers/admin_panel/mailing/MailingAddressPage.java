package com.jvm_bloggers.admin_panel.mailing;

import com.jvm_bloggers.admin_panel.panels.CustomFeedbackPanel;
import com.jvm_bloggers.admin_panel.panels.CustomPagingNavigator;
import com.jvm_bloggers.core.mailing.domain.MailingAddress;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.bean.validation.PropertyValidator;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.function.Supplier;

@Slf4j
@MountPath("mailing-address")
public class MailingAddressPage extends AbstractMailingPage {

    public static final String MAILING_ADDRESS_FORM_ID = "mailingAddressForm";
    public static final String SAVE_BUTTON_ID = "saveButton";
    public static final String CANCEL_BUTTON_ID = "cancelButton";
    public static final String ADDRESS_INPUT_ID = "address";
    public static final String ID_INPUT_ID = "id";
    public static final String FEEDBACK_PANEL_ID = "feedbackPanel";
    public static final String ACTION_PANEL_ID = "actionPanel";
    public static final String PAGEABLE_LIST_ID = "pageable";
    public static final Supplier<MailingAddress> DEFAULT_MODEL = () -> new MailingAddress("");

    private Form<MailingAddress> mailingAddressForm;

    @SpringBean
    private MailingAddressPageRequestHandler mailingAddressPageRequestHandler;

    public MailingAddressPage() {
        addForm();
        addMailingAddressList();
    }

    private void addForm() {
        FormComponent idField = (FormComponent) new TextField(ID_INPUT_ID).setEnabled(false);
        FormComponent addressField = (FormComponent) new TextField<String>(ADDRESS_INPUT_ID)
                .add(new PropertyValidator<MailingAddress>());
        mailingAddressForm = new Form<>(MAILING_ADDRESS_FORM_ID);
        mailingAddressForm.add(idField);
        mailingAddressForm.add(addressField);
        mailingAddressForm.add(new CustomFeedbackPanel(FEEDBACK_PANEL_ID));
        mailingAddressForm.add(new MailingAddressUniquenessValidator(idField, addressField));
        mailingAddressForm.setModel(CompoundPropertyModel.of(Model.of(DEFAULT_MODEL.get())));
        mailingAddressForm.setOutputMarkupId(true);
        add(mailingAddressForm);
        addCancelButton();
        addSubmitFormButton();
    }

    private void addCancelButton() {
        AjaxButton cancelButton = new AjaxButton(CANCEL_BUTTON_ID, mailingAddressForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                mailingAddressForm.setModelObject(DEFAULT_MODEL.get());
                mailingAddressForm.clearInput();
                target.add(form);
            }
        };
        cancelButton.setDefaultFormProcessing(false);
        mailingAddressForm.add(cancelButton);
    }

    private void addSubmitFormButton() {
        AjaxButton saveButton = new AjaxButton(SAVE_BUTTON_ID, mailingAddressForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                MailingAddress mailingAddress = mailingAddressForm.getModelObject();
                mailingAddressPageRequestHandler.save(mailingAddress);
                success("Mailing address '" + mailingAddress.getAddress() + "' saved successfully");
                mailingAddressForm.setModelObject(DEFAULT_MODEL.get());
                target.add(form);
            }

            @Override
            protected void onError(AjaxRequestTarget target, Form<?> form) {
                target.add(form);
            }
        };
        mailingAddressForm.add(saveButton);
    }

    private void addMailingAddressList() {
        final DataView<MailingAddress> dataView = new DataView<MailingAddress>(
                PAGEABLE_LIST_ID, mailingAddressPageRequestHandler) {
            @Override
            protected void populateItem(Item<MailingAddress> item) {
                MailingAddress modelObject = item.getModelObject();
                item.add(new Label("id", modelObject.getId()));
                item.add(new Label("address", modelObject.getAddress()));
                item.add(new MailingAddressActionPanel(
                        ACTION_PANEL_ID, mailingAddressForm, item.getModel()));
            }
        };

        dataView.setItemsPerPage(defaultPaginationSize);
        dataView.setOutputMarkupId(true);
        mailingAddressForm.add(dataView);
        mailingAddressForm.add(new CustomPagingNavigator("navigator", dataView));
    }

}
