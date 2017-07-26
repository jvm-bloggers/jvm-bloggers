package com.jvm_bloggers.frontend.admin_area;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("social-channels")
public class AdminSocialChannelsPage extends AbstractAdminPage {

    @SpringBean
    private AdminSocialChannelsPageBackingBean backingBean;

    private String link;
    private String message;

    public AdminSocialChannelsPage() {
        initGui();
    }

    private void initGui() {
        Form<AdminSocialChannelsPage>
            facebookPostForm =
            new Form<AdminSocialChannelsPage>("facebookPostForm",
                new CompoundPropertyModel<AdminSocialChannelsPage>(this));
        add(facebookPostForm);
        Label linkLabel = new Label("linkLabel", "Link");
        facebookPostForm.add(linkLabel);
        TextField<String> linkField = new TextField<String>("link");
        facebookPostForm.add(linkField);
        Label messageLabel = new Label("messageLabel", "Message");
        facebookPostForm.add(messageLabel);
        TextArea<String> messageField = new TextArea<String>("message");
        facebookPostForm.add(messageField);
        Button submitButton = new Button("submitButton") {
            @Override
            public void onSubmit() {

                backingBean.createNewFacebookPost(link, message);
                getSession().info("Facebook post saved");
            }
        };
        facebookPostForm.add(submitButton);
    }
}
