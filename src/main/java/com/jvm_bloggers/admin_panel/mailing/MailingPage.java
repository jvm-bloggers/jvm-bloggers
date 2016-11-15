package com.jvm_bloggers.admin_panel.mailing;

import com.google.common.collect.Lists;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.WysiwygEditor;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.toolbar.DefaultWysiwygToolbar;
import com.jvm_bloggers.admin_panel.panels.CustomFeedbackPanel;
import com.jvm_bloggers.core.metadata.Metadata;
import com.jvm_bloggers.core.metadata.MetadataKeys;
import com.jvm_bloggers.core.metadata.MetadataRepository;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.extensions.ajax.markup.html.modal.ModalWindow;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.List;


@MountPath("mailing-template")
@AuthorizeInstantiation(Roles.ADMIN)
public class MailingPage extends AbstractMailingPage {

    public static final String WYSIWYG_ID = "wysiwyg";
    public static final String MAILING_TEMPLATE_FORM_ID = "mailingTemplateForm";
    public static final String RESET_MAILING_TEMPLATE_BUTTON_ID = "resetMailingTemplateButton";
    public static final String SEND_TEST_MAIL_BUTTON_ID = "sendTestMailButton";
    public static final String MAILING_SECTION_TO_EDIT_DROPDOWN_ID = "mailingToEditDropdown";

    private static List<String> TEMPLATE_SECTION_KEYS = Lists.newArrayList(
        MetadataKeys.MAILING_TEMPLATE,
        MetadataKeys.MAILING_GREETING,
        MetadataKeys.HEADING_TEMPLATE,
        MetadataKeys.VARIA_TEMPLATE,
        MetadataKeys.MAILING_SIGNATURE
    );
    private DropDownChoice<String> newsletterSectionToEditDropdown;
    private CustomFeedbackPanel feedback;
    private WysiwygEditor wysiwygEditor;
    private Form<Metadata> mailingTemplateForm;

    @SpringBean
    private MetadataRepository metadataRepository;
    @SpringBean
    private MailingPageRequestHandler requestHandler;

    public MailingPage() {
        addFeedbackPanel();
        addForm();
        addNewsletterSectionToEditDropdown();
        addWysiwygEditor();
        addSaveButton();
        addResetTemplateButton();
        addPreviewTemplateModal();
        addSendTestMailButton();
    }

    private void addFeedbackPanel() {
        feedback = new CustomFeedbackPanel("feedback");
        add(feedback);
    }

    private void addForm() {
        Metadata initialSectionToEdit = metadataRepository
            .findByName(TEMPLATE_SECTION_KEYS.get(0));
        mailingTemplateForm = new Form<>(MAILING_TEMPLATE_FORM_ID, Model.of(initialSectionToEdit));
        mailingTemplateForm.setOutputMarkupId(true);
        add(mailingTemplateForm);
    }

    private void addWysiwygEditor() {
        DefaultWysiwygToolbar toolbar = new DefaultWysiwygToolbar("toolbar");
        wysiwygEditor = new WysiwygEditor(
            WYSIWYG_ID, new PropertyModel<>(mailingTemplateForm.getModel(), "value"),
            toolbar
        );
        wysiwygEditor.setOutputMarkupId(true);
        mailingTemplateForm.add(toolbar, wysiwygEditor);
    }

    private void addNewsletterSectionToEditDropdown() {
        newsletterSectionToEditDropdown = new DropDownChoice<>(
            MAILING_SECTION_TO_EDIT_DROPDOWN_ID,
            Model.of(mailingTemplateForm.getModelObject().getName()),
            TEMPLATE_SECTION_KEYS
        );
        newsletterSectionToEditDropdown.add(new AjaxFormComponentUpdatingBehavior("change") {
                @Override
                protected void onUpdate(AjaxRequestTarget target) {
                    String metadata = newsletterSectionToEditDropdown.getModelObject();
                    Metadata metadataToEdit = metadataRepository.findByName(metadata);
                    mailingTemplateForm.setModelObject(metadataToEdit);
                    target.add(mailingTemplateForm);
                    target.add(wysiwygEditor);
                }
            }
        );
        mailingTemplateForm.add(newsletterSectionToEditDropdown);
    }

    private void addSaveButton() {
        AjaxButton saveButton = new AjaxButton("saveButton", mailingTemplateForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                persistChangesInMetadata(target);
            }

            private void persistChangesInMetadata(AjaxRequestTarget target) {
                Metadata metadataToUpdate = mailingTemplateForm.getModelObject();
                metadataRepository.save(metadataToUpdate);
                success("Mail template for " + metadataToUpdate.getName() + " saved successfully");
                target.add(feedback);
            }
        };
        mailingTemplateForm.add(saveButton);
    }

    private void addResetTemplateButton() {
        AjaxButton resetTemplateButton =
            new AjaxButton(RESET_MAILING_TEMPLATE_BUTTON_ID, mailingTemplateForm) {
                @Override
                protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                    String defaultMailingTemplate = metadataRepository
                        .findByName(MetadataKeys.DEFAULT_MAILING_TEMPLATE).getValue();
                    Metadata metadataToUpdate = mailingTemplateForm.getModelObject();
                    metadataToUpdate.setValue(defaultMailingTemplate);
                    metadataRepository.save(metadataToUpdate);
                    success("Mail template for " + metadataToUpdate.getName()
                        + " set to default value");
                    target.add(mailingTemplateForm);
                    target.add(wysiwygEditor);
                }

                @Override
                public boolean isVisible() {
                    return TEMPLATE_SECTION_KEYS.get(0)
                        .equals(newsletterSectionToEditDropdown.getModelObject());
                }
            };
        mailingTemplateForm.add(resetTemplateButton);
    }

    private void addPreviewTemplateModal() {
        ModalWindow mailingPreviewModalWindow = new ModalWindow("mailingPreviewModal");
        mailingTemplateForm.add(mailingPreviewModalWindow);

        mailingPreviewModalWindow
            .setContent(new MailingTemplatePreviewPanel(mailingPreviewModalWindow.getContentId()));
        mailingPreviewModalWindow.setTitle("Mailing preview");
        mailingPreviewModalWindow.setCookieName("mailing-preview-modal");

        AjaxButton previewButton = new AjaxButton("previewButton", mailingTemplateForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                mailingPreviewModalWindow.show(target);
            }
        };
        mailingTemplateForm.add(previewButton);
    }

    private void addSendTestMailButton() {
        AjaxButton sendTestMailButton = new AjaxButton(
            SEND_TEST_MAIL_BUTTON_ID,
            mailingTemplateForm
        ) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                String testEmailRecipient = requestHandler.sendTestEmail();
                success("Test email sent to " + testEmailRecipient + "!");
                target.add(feedback);
            }
        };
        mailingTemplateForm.add(sendTestMailButton);
    }

}
