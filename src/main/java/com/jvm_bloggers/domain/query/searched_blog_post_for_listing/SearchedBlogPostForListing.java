package com.jvm_bloggers.domain.query.searched_blog_post_for_listing;

import java.io.Serializable;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class SearchedBlogPostForListing implements Serializable {

  String url;
  String title;
  String author;
  String authorTwitterHandle;
  LocalDateTime publishedTime;

}
