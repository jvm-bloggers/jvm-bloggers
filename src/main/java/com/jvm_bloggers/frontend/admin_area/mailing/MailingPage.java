package com.jvm_bloggers.frontend.admin_area.mailing;

import com.google.common.collect.Lists;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.WysiwygEditor;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.toolbar.DefaultWysiwygToolbar;
import com.jvm_bloggers.entities.metadata.Metadata;
import com.jvm_bloggers.entities.metadata.MetadataKeys;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import com.jvm_bloggers.frontend.common_components.NonNullWysiwygEditor;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.modal.ModalWindow;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.List;

@MountPath("mailing-template")
public class MailingPage extends AbstractMailingPage {

    public static final String WYSIWYG_ID = "wysiwyg";
    public static final String MAILING_TEMPLATE_FORM_ID = "mailingTemplateForm";
    public static final String RESET_MAILING_TEMPLATE_BUTTON_ID = "resetMailingTemplateButton";
    public static final String SEND_TEST_MAIL_BUTTON_ID = "sendTestMailButton";
    public static final String MAILING_SECTION_TO_EDIT_DROPDOWN_ID = "mailingToEditDropdown";
    public static final String VARIA_SUGGESTION_PANEL_ID = "variaSuggestionPanel";
    public static final String SAVE_BUTTON_ID = "saveButton";

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
    private VariaSuggestionListPanel variaSuggestionListPanel;

    @SpringBean
    private MailingPageBackingBean backingBean;

    public MailingPage() {
        addFeedbackPanel();
        addForm();
        addNewsletterSectionToEditDropdown();
        addWysiwygEditor();
        addVariaSugesstionList();
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
        Metadata initialSectionToEdit = backingBean
            .findMetadataByName(TEMPLATE_SECTION_KEYS.get(0));
        mailingTemplateForm = new Form<>(MAILING_TEMPLATE_FORM_ID, Model.of(initialSectionToEdit));
        mailingTemplateForm.setOutputMarkupId(true);
        add(mailingTemplateForm);
    }

    private void addWysiwygEditor() {
        DefaultWysiwygToolbar toolbar = new DefaultWysiwygToolbar("toolbar");
        wysiwygEditor = new NonNullWysiwygEditor(
            WYSIWYG_ID, new PropertyModel<>(mailingTemplateForm.getModel(), "value"),
            toolbar
        );
        wysiwygEditor.setOutputMarkupId(true);
        mailingTemplateForm.add(toolbar, wysiwygEditor);
    }

    private void addVariaSugesstionList() {
        variaSuggestionListPanel = new VariaSuggestionListPanel(
            VARIA_SUGGESTION_PANEL_ID,
            backingBean,
            defaultPaginationSize) {
            @Override
            public boolean isVisible() {
                return TEMPLATE_SECTION_KEYS.get(3)
                    .equals(newsletterSectionToEditDropdown.getModelObject());
            }
        };
        variaSuggestionListPanel.setOutputMarkupId(true);
        variaSuggestionListPanel.setOutputMarkupPlaceholderTag(true);
        add(variaSuggestionListPanel);
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
                    Metadata metadataToEdit = backingBean.findMetadataByName(metadata);
                    mailingTemplateForm.setModelObject(metadataToEdit);
                    target.add(mailingTemplateForm);
                    target.add(wysiwygEditor);
                    target.add(variaSuggestionListPanel);
                }
            }
        );
        mailingTemplateForm.add(newsletterSectionToEditDropdown);
    }

    private void addSaveButton() {
        AjaxButton saveButton = new AjaxButton(SAVE_BUTTON_ID, mailingTemplateForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target) {
                persistChangesInMetadata(target);
            }

            private void persistChangesInMetadata(AjaxRequestTarget target) {
                Metadata metadataToUpdate = mailingTemplateForm.getModelObject();
                backingBean.saveMetadata(metadataToUpdate);
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
                protected void onSubmit(AjaxRequestTarget target) {
                    String defaultMailingTemplate = backingBean
                        .findMetadataByName(MetadataKeys.DEFAULT_MAILING_TEMPLATE).getValue();
                    Metadata metadataToUpdate = mailingTemplateForm.getModelObject();
                    metadataToUpdate.setValue(defaultMailingTemplate);
                    backingBean.saveMetadata(metadataToUpdate);
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
            protected void onSubmit(AjaxRequestTarget target) {
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
            protected void onSubmit(AjaxRequestTarget target) {
                String testEmailRecipient = backingBean.sendTestEmail();
                success("Test email sent to " + testEmailRecipient + "!");
                target.add(feedback);
            }
        };
        mailingTemplateForm.add(sendTestMailButton);
    }

}
