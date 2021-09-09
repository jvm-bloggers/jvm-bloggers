package com.jvm_bloggers.frontend.public_area.search_posts;

import java.util.regex.Pattern;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;

public class SearchPhraseValidator implements IValidator<String> {

  private static final String ERROR_MESSAGE = "Wyszukiwana fraza powinna zaczynać się od przynajmniej 3 liter";
  private static final Pattern PATTERN = Pattern.compile("^[a-zA-Z]{3}.*$");

  @Override
  public void validate(IValidatable<String> validatable) {
    String phrase = validatable.getValue().trim();
    if (!PATTERN.matcher(phrase).matches()) {
      validatable.error(new ValidationError(ERROR_MESSAGE));
    }
  }
}
