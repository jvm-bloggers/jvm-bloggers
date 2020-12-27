package com.jvm_bloggers.frontend.public_area.search_posts;

import com.googlecode.wicket.jquery.ui.form.autocomplete.AutoCompleteTextField;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;
import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("wyszukaj")
@Slf4j
public class SearchPostsPage extends AbstractFrontendPage {

  private final String SEARCH_FORM = "searchForm";
  private final String SEARCH_PHRASE = "searchPhrase";
  private final String SUBMIT_ID = "submit";

  private final Form<SearchPostsModel> searchPostsModelForm;

  @SpringBean
  private SearchPostsPageBackingBean backingBean;

  public SearchPostsPage() {
    searchPostsModelForm = new Form<>(SEARCH_FORM, new CompoundPropertyModel<>(new SearchPostsModel()));
    add(searchPostsModelForm);
    initSearchPhraseInput();
    initSubmitButton();
  }

  private void initSearchPhraseInput() {
    var searchInput = new TextField<String>(SEARCH_PHRASE);
    searchPostsModelForm.add(searchInput);
  }

  private void initSubmitButton() {
    var searchSubmitButton = new AjaxButton(SUBMIT_ID, searchPostsModelForm) {
      @Override
      protected void onSubmit(AjaxRequestTarget target) {
        log.info("submit an ajax request {} ", searchPostsModelForm.getModelObject());
        super.onSubmit(target);
      }

      @Override
      protected void onError(AjaxRequestTarget target) {
        log.error("an ajax request error");
        super.onError(target);
      }
    };
    searchPostsModelForm.add(searchSubmitButton);
  }

  @Override
  protected String getPageTitle() {
    return "Wyszukaj posty";
  }
}
