package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.WysiwygEditor;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.toolbar.DefaultWysiwygToolbar;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.extensions.ajax.markup.html.modal.ModalWindow;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.settings.Setting;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingRepository;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AbstractAdminPage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;


@Slf4j
@AuthorizeInstantiation(Roles.ADMIN)
public class MailingPage extends AbstractAdminPage {

    @SpringBean
    private SettingRepository settingRepository;

    public MailingPage() {
        final CustomFeedbackPanel feedback = new CustomFeedbackPanel("feedback");
        add(feedback);

        Form<Setting> mailingTemplateForm = new Form<>("mailingTemplateForm", new MailingTemplateModel(settingRepository));
        mailingTemplateForm.setOutputMarkupId(true);
        add(mailingTemplateForm);

        DefaultWysiwygToolbar toolbar = new DefaultWysiwygToolbar("toolbar");
        final WysiwygEditor editor = new WysiwygEditor("wysiwyg", new PropertyModel<>(mailingTemplateForm.getModel(), "value"), toolbar);
        mailingTemplateForm.add(toolbar, editor);

        // Buttons //
        AjaxButton saveButton = new AjaxButton("saveButton", mailingTemplateForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                Setting setting = (Setting) form.getModelObject();
                log.info("value = " + setting.getValue());
                settingRepository.save(setting);
                success("Mail template saved successfully");
                target.add(feedback);
            }
        };
        mailingTemplateForm.add(saveButton);

        Button sendTestMailButton = new Button("sendTestMailButton");
        mailingTemplateForm.add(sendTestMailButton);

        addPreviewTemplateModal(mailingTemplateForm);
    }

    private void addPreviewTemplateModal(final Form<Setting> mailingTemplateForm) {
        ModalWindow mailingPreviewModalWindow = new ModalWindow("mailingPreviewModal");
        mailingTemplateForm.add(mailingPreviewModalWindow);

        mailingPreviewModalWindow.setContent(new MailingTemplatePreviewPanel(mailingPreviewModalWindow.getContentId()));
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

}
