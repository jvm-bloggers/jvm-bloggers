package com.jvm_bloggers.frontend.admin_area;


import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;

import org.apache.wicket.request.mapper.parameter.PageParameters;
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
        Form<AdminSocialChannelsPage> facebookPostForm = new Form<AdminSocialChannelsPage>("facebookPostForm",
                new CompoundPropertyModel<AdminSocialChannelsPage>(this));  // (1)
        add(facebookPostForm);
        Label linkLabel = new Label("linkLabel", "Link");    // (2)
        facebookPostForm.add(linkLabel);
        TextField<String> linkField = new TextField<String>("link");
        facebookPostForm.add(linkField);
        Label messageLabel = new Label("messageLabel", "Message");    // (2)
        facebookPostForm.add(messageLabel);
        TextArea<String> messageField = new TextArea<String>("message");   // (3)
        facebookPostForm.add(messageField);
        Button submitButton = new Button("submitButton") {          // (4)
            @Override
            public void onSubmit() {

                backingBean.createNewFacebookPost(link, message);
                System.out.println("OnSubmit, name = " + messageField);
            }
        };
        facebookPostForm.add(submitButton);
    }
}
//
//    private String message;
//    private String link;
//
//    public AdminSocialChannelsPage(final PageParameters parameters) {
//        Form form = new Form("form") {
//                    @Override
//                    protected void onSubmit() {
//                        backingBean.createNewFacebookPost(message,link);
//                        setResponsePage(AdminSocialChannelsPage.class);
//                    }};
//        form.add(new TextArea("message"));
//        form.add(new TextField("link"));
//
//        add(form.setDefaultModel(new CompoundPropertyModel(this)));
//
//    }




