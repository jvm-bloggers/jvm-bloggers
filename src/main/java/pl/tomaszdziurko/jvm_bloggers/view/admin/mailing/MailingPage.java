package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import com.google.common.collect.Lists;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.WysiwygEditor;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.toolbar.DefaultWysiwygToolbar;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.extensions.ajax.markup.html.modal.ModalWindow;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

import pl.tomaszdziurko.jvm_bloggers.metadata.Metadata;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AbstractAdminPage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;

import java.util.List;


@Slf4j
@AuthorizeInstantiation(Roles.ADMIN)
public class MailingPage extends AbstractAdminPage {

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
    private Form<String> mailingTemplateForm;

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
        mailingTemplateForm = new Form<>("mailingTemplateForm");
        mailingTemplateForm.setOutputMarkupId(true);
        add(mailingTemplateForm);
    }

    private void addWysiwygEditor() {
        DefaultWysiwygToolbar toolbar = new DefaultWysiwygToolbar("toolbar");
        wysiwygEditor = new WysiwygEditor("wysiwyg", new Model<String>() {
            @Override
            public String getObject() {
                String metadata = newsletterSectionToEditDropdown.getModelObject();
                return metadataRepository.findByName(metadata).getValue();
            }
        }, toolbar);

        wysiwygEditor.setOutputMarkupId(true);
        mailingTemplateForm.add(toolbar, wysiwygEditor);
    }

    private void addNewsletterSectionToEditDropdown() {
        Metadata initialSectionToEdit = metadataRepository
            .findByName(TEMPLATE_SECTION_KEYS.get(0));
        newsletterSectionToEditDropdown = new DropDownChoice<>(
            "mailingToEditDropdown", TEMPLATE_SECTION_KEYS);
        newsletterSectionToEditDropdown.setModel(Model.of(initialSectionToEdit.getName()));
        newsletterSectionToEditDropdown.add(new AjaxFormComponentUpdatingBehavior("change") {
                @Override
                protected void onUpdate(AjaxRequestTarget target) {
                    target.add(mailingTemplateForm);
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
                String metadataName = newsletterSectionToEditDropdown.getModelObject();
                String mailingSectionContent = wysiwygEditor.getModelObject();
                Metadata metadataToUpdate = metadataRepository.findByName(metadataName);
                metadataToUpdate.setValue(mailingSectionContent);
                metadataRepository.save(metadataToUpdate);
                success("Mail template for " + metadataName + " saved successfully");
                target.add(feedback);
            }
        };
        mailingTemplateForm.add(saveButton);
    }

    private void addResetTemplateButton() {
        AjaxButton resetTemplateButton =
            new AjaxButton("resetMailingTemplateButton", mailingTemplateForm) {
                @Override
                protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
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
        AjaxButton sendTestMailButton = new AjaxButton("sendTestMailButton", mailingTemplateForm) {
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
