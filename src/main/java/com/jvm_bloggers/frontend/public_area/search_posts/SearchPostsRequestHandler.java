package com.jvm_bloggers.frontend.public_area.search_posts;

import com.google.common.base.Stopwatch;
import com.jvm_bloggers.domain.query.searched_blog_post_for_listing.SearchedBlogPostForListing;
import com.jvm_bloggers.domain.query.searched_blog_post_for_listing.SearchedBlogPostForListingQuery;
import java.util.Collections;
import java.util.Iterator;
import java.util.concurrent.TimeUnit;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

@Slf4j
@RequiredArgsConstructor
public final class SearchPostsRequestHandler implements IDataProvider<SearchedBlogPostForListing> {

  private final Form<SearchPostsModel> searchPostsModelForm;
  private final SearchedBlogPostForListingQuery query;
  private final int pageSize;

  @Override
  public Iterator<? extends SearchedBlogPostForListing> iterator(long first, long count) {
    log.debug("Refreshing data, first {}, count {}", first, count);
    var searchPhrase = getSearchPhrase();

    if (StringUtils.isEmpty(searchPhrase)) {
      return Collections.emptyIterator();
    }

    int page = (int) (first / pageSize);
    var stopWatch = Stopwatch.createStarted();
    var iterator = query
        .findByTitleOrTag(searchPhrase, page, pageSize)
        .iterator();
    long elapsed = stopWatch.stop().elapsed(TimeUnit.MILLISECONDS);
    log.debug("iterator() execution time = {} ms", elapsed);
    return iterator;
  }

  @Override
  public long size() {
    var stopwatch = Stopwatch.createStarted();
    var searchPhrase = getSearchPhrase();

    if (StringUtils.isEmpty(searchPhrase)) {
      return 0;
    }

    var size = query.countByTitleOrTag(searchPhrase);
    long elapsed = stopwatch.stop().elapsed(TimeUnit.MILLISECONDS);
    log.debug("Size() execution time = {} ms", elapsed);
    return size;
  }

  @Override
  public IModel<SearchedBlogPostForListing> model(SearchedBlogPostForListing object) {
    return Model.of(object);
  }

  private String getSearchPhrase() {
    return searchPostsModelForm.getModelObject().getSearchPhrase();
  }
}
