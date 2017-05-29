package com.jvm_bloggers.frontend.admin_area.mailing;

import com.jvm_bloggers.entities.mailing_address.MailingAddress;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import com.jvm_bloggers.frontend.admin_area.panels.CustomPagingNavigator;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.bean.validation.PropertyValidator;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
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
        TextField<String> idField = new TextField<>(ID_INPUT_ID);
        idField.setEnabled(false);
        TextField<String> addressField = new TextField<>(ADDRESS_INPUT_ID);
        addressField.add(new PropertyValidator<MailingAddress>());
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
            protected void onSubmit(AjaxRequestTarget target) {
                mailingAddressForm.setModelObject(DEFAULT_MODEL.get());
                mailingAddressForm.clearInput();
                target.add(getForm());
            }
        };
        cancelButton.setDefaultFormProcessing(false);
        mailingAddressForm.add(cancelButton);
    }

    private void addSubmitFormButton() {
        AjaxButton saveButton = new AjaxButton(SAVE_BUTTON_ID, mailingAddressForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target) {
                MailingAddress mailingAddress = mailingAddressForm.getModelObject();
                mailingAddressPageRequestHandler.save(mailingAddress);
                success("Mailing address '" + mailingAddress.getAddress() + "' saved successfully");
                mailingAddressForm.setModelObject(DEFAULT_MODEL.get());
                target.add(getForm());
            }

            @Override
            protected void onError(AjaxRequestTarget target) {
                target.add(getForm());
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
