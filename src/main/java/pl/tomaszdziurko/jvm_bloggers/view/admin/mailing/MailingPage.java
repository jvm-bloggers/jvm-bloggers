package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.WysiwygEditor;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.toolbar.DefaultWysiwygToolbar;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.extensions.ajax.markup.html.modal.ModalWindow;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import pl.tomaszdziurko.jvm_bloggers.settings.Metadata;
import pl.tomaszdziurko.jvm_bloggers.settings.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AbstractAdminPage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;


@Slf4j
@AuthorizeInstantiation(Roles.ADMIN)
public class MailingPage extends AbstractAdminPage {

    private final CustomFeedbackPanel feedback;
    private final WysiwygEditor editor;
    @SpringBean
    private MetadataRepository metadataRepository;

    @SpringBean
    private MailingPageRequestHandler requestHandler;

    public MailingPage() {
        feedback = new CustomFeedbackPanel("feedback");
        add(feedback);

        Form<Metadata> mailingTemplateForm = new Form<>("mailingTemplateForm",
                new MailingTemplateModel(metadataRepository));
        mailingTemplateForm.setOutputMarkupId(true);
        add(mailingTemplateForm);

        DefaultWysiwygToolbar toolbar = new DefaultWysiwygToolbar("toolbar");
        editor = new WysiwygEditor("wysiwyg", new PropertyModel<>(mailingTemplateForm.getModel(),
            "value"), toolbar);
        editor.setOutputMarkupId(true);
        mailingTemplateForm.add(toolbar, editor);

        AjaxButton saveButton = new AjaxButton("saveButton", mailingTemplateForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                Metadata metadata = (Metadata) form.getModelObject();
                log.info("value = " + metadata.getValue());
                metadataRepository.save(metadata);
                success("Mail template saved successfully");
                target.add(feedback);
            }
        };
        mailingTemplateForm.add(saveButton);

        addResetTemplateButton(mailingTemplateForm);

        addPreviewTemplateModal(mailingTemplateForm);
        addSendTestMailButton(mailingTemplateForm);
    }

    private void addResetTemplateButton(Form<Metadata> mailingTemplateForm) {
        AjaxButton resetTemplateButton =
                new AjaxButton("resetMailingTemplateButton", mailingTemplateForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                String defaultMailingTemplate = requestHandler.loadDefaultMailingTemplate();
                Metadata mailingTemplate =
                        (Metadata) mailingTemplateForm.getDefaultModel().getObject();
                mailingTemplate.setValue(defaultMailingTemplate);
                warn("Mailing template reset to default value. To persist changes please click "
                        + "'Save' button");
                target.add(feedback);
                target.add(editor);
            }
        };
        mailingTemplateForm.add(resetTemplateButton);
    }

    private void addPreviewTemplateModal(final Form<Metadata> mailingTemplateForm) {
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

    private void addSendTestMailButton(Form<Metadata> mailingTemplateForm) {
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
