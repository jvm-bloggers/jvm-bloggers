package com.jvm_bloggers.entities.blog_post;

import java.util.List;
import org.springframework.transaction.annotation.Transactional;

public interface BlogPostTextSearchRepository {

  @Transactional
  List<BlogPost> findByTagOrTitle(String searchPhrase, int page, int pageSize);

}
