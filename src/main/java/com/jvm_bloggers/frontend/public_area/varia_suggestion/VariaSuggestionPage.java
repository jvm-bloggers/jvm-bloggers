package com.jvm_bloggers.frontend.public_area.varia_suggestion;

import com.jvm_bloggers.frontend.common_components.toastr.ToastrBehavior;
import com.jvm_bloggers.frontend.common_components.toastr.ToastrBuilder;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;

import io.vavr.collection.Stream;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.feedback.FeedbackMessage;
import org.apache.wicket.feedback.FeedbackMessages;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.UrlValidator;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("varia-suggestion")
public class VariaSuggestionPage extends AbstractFrontendPage {

    public static final String FORM_ID = "variaSuggestionForm";
    public static final String AUTHOR_ID = "author";
    public static final String REASON_ID = "reason";
    public static final String SUBMIT_ID = "submit";
    public static final String URL_ID = "url";
    public static final String[] ALLOWED_URL_SCHEMAS = {"http", "https", "HTTP", "HTTPS"};


    @SpringBean
    private VariaSuggestionPageBackingBean backingBean;

    private Form<VariaSuggestionModel> variaSuggestionForm;

    public VariaSuggestionPage() {
        addToastrBehavior();
        createForm();
        createSubmitButton();
    }

    private void addToastrBehavior() {
        add(new ToastrBehavior());
    }

    private void createForm() {
        variaSuggestionForm = new Form<>(FORM_ID, new CompoundPropertyModel<>(new VariaSuggestionModel()));

        variaSuggestionForm.add(new TextField<String>(URL_ID)
            .setRequired(true)
            .add(new UrlValidator(ALLOWED_URL_SCHEMAS)));
        variaSuggestionForm.add(new TextArea<>(REASON_ID).setRequired(true));
        variaSuggestionForm.add(new TextField<>(AUTHOR_ID));
        variaSuggestionForm.setOutputMarkupId(true);
        add(variaSuggestionForm);
    }

    private void createSubmitButton() {
        AjaxButton submitButton = new AjaxButton(SUBMIT_ID, variaSuggestionForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target) {
                backingBean.createVariaSuggestion(variaSuggestionForm.getModelObject());
                target.appendJavaScript(
                    ToastrBuilder.success("Propozycja zapisana. Dziękujęmy za pomoc :)")
                );
                variaSuggestionForm.setDefaultModelObject(new VariaSuggestionModel());
                target.add(variaSuggestionForm);
            }

            @Override
            protected void onError(AjaxRequestTarget target) {
                target.appendJavaScript(ToastrBuilder.error(
                    Stream.ofAll(getForm().streamChildren())
                        .map(Component::getFeedbackMessages)
                        .flatMap(FeedbackMessages::toList)
                        .filter(FeedbackMessage::isError)
                        .map(FeedbackMessage::getMessage)
                        .mkString("<br />")
                ));
            }
        };
        variaSuggestionForm.add(submitButton);
    }

    @Override
    protected String getPageTitle() {
        return "Propozycje do sekcji Varia";
    }
}
