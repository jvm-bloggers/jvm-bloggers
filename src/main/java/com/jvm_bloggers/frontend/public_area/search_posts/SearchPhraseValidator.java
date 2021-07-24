package com.jvm_bloggers.frontend.public_area.search_posts;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;

public class SearchPhraseValidator implements IValidator<String> {

  @Override
  public void validate(IValidatable<String> validatable) {
    String phrase = validatable.getValue();
    if (StringUtils.isEmpty(phrase) || phrase.length() < 3) {
      validatable.error(new ValidationError("Search phrase should contains at last 3 characters"));
    }
  }
}
