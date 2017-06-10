package com.jvm_bloggers.frontend.admin_area.mailing;

import com.jvm_bloggers.entities.mailing_address.MailingAddressRepository;

import io.vavr.control.Option;

import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Args;

public class MailingAddressUniquenessValidator extends AbstractFormValidator {

    private final FormComponent<?> idComponent;
    private final FormComponent<?> addressComponent;

    @SpringBean
    private MailingAddressRepository mailingAddressRepository;

    public MailingAddressUniquenessValidator(
            FormComponent<?> idComponent,
            FormComponent<?> addressComponent) {
        Args.notNull(idComponent, "idComponent");
        Args.notNull(addressComponent, "addressComponent");

        this.idComponent = idComponent;
        this.addressComponent = addressComponent;
    }

    @Override
    public FormComponent<?>[] getDependentFormComponents() {
        return new FormComponent[]{idComponent, addressComponent};
    }

    @Override
    public void validate(Form<?> form) {
        Long id = Option.of(idComponent.getValue())
            .filter(value -> !value.isEmpty())
            .map(Long::valueOf)
            .getOrElse(() -> null);
        if (mailingAddressRepository.addressExistsIgnoringId(addressComponent.getValue(), id)) {
            error(addressComponent);
        }
    }
}
