package com.jvm_bloggers.entities.blog_post;

import java.util.List;

public interface BlogPostTextSearchRepository {

  List<BlogPost> findApprovedPostsByTagOrTitle(String searchPhrase, int page, int pageSize);

  long countApprovedPostsByTagOrTitle(String  searchPhrase);

}
