package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.entities.fb.FacebookPost;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.bean.validation.PropertyValidator;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.UrlValidator;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("social-channels")
public class AdminSocialChannelsPage extends AbstractAdminPage {

    public static final String FACEBOOK_POST_FORM_ID = "facebookPostForm";
    public static final String SAVE_BUTTON_ID = "submitButton";
    public static final String FEEDBACK_PANEL_ID = "feedbackPanel";
    public static final String LINK_INPUT_ID = "link";
    public static final String MESSAGE_INPUT_ID = "message";

    private Form<AdminSocialChannelsPage> facebookPostForm;
    @SpringBean
    private AdminSocialChannelsPageBackingBean backingBean;

    private String link;
    private String message;

    public AdminSocialChannelsPage() {
        addForm();
    }

    private void addForm() {
        facebookPostForm = new Form<AdminSocialChannelsPage>(
            FACEBOOK_POST_FORM_ID, new CompoundPropertyModel<AdminSocialChannelsPage>(this)
        );
        TextField<String> linkField = new TextField<>(LINK_INPUT_ID);
        TextArea<String> messageField = new TextArea<>(MESSAGE_INPUT_ID);
        linkField.add(new PropertyValidator<FacebookPost>());
        messageField.add(new PropertyValidator<FacebookPost>());
        facebookPostForm.add(linkField.setRequired(true).add(new UrlValidator()));
        facebookPostForm.add(messageField.setRequired(true));
        facebookPostForm.add(new CustomFeedbackPanel(FEEDBACK_PANEL_ID));
        add(facebookPostForm);
        addSubmitFormButton();
    }

    private void addSubmitFormButton() {
        AjaxButton submitButton = new AjaxButton(SAVE_BUTTON_ID, facebookPostForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target) {
                backingBean.createNewFacebookPost(link, message);
                success("Facebook post saved successfully");
                target.add(getForm());
            }

            @Override
            protected void onError(AjaxRequestTarget target) {
                target.add(getForm());
            }
        };
        facebookPostForm.add(submitButton);
    }
}
