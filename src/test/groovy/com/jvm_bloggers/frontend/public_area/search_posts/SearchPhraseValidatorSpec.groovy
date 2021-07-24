package com.jvm_bloggers.frontend.public_area.search_posts


import org.apache.wicket.validation.IValidatable
import org.apache.wicket.validation.Validatable
import spock.lang.Specification

class SearchPhraseValidatorSpec extends Specification {

  SearchPhraseValidator validator = new SearchPhraseValidator()

  def "check if search phrase is valid"(String searchPhrase, boolean isValid) {
    given:
    IValidatable<String> validatable = new Validatable<>(searchPhrase)
    when:
    validator.validate(validatable)
    then:
    validatable.isValid() == isValid
    where:
    searchPhrase | isValid
    "hibernate"  | true
    "hi bernate" | false
    "s pring"    | false
    "Spring JPA" | true
    "JAVA 16"    | true
    "16 JAVA"    | false
    "%%%"        | false
  }
}
