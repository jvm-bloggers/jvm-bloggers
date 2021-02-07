package com.jvm_bloggers.domain.query.searched_blog_post_for_listing;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import io.vavr.collection.Array;
import io.vavr.collection.Traversable;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@AllArgsConstructor
@Service
@Transactional(readOnly = true)
public class SearchedBlogPostForListingQuery {

  private BlogPostRepository blogPostRepository;

  public Traversable<SearchedBlogPostForListing> findByTitleOrTag(String searchPhrase, int page, int pageSize) {
    return blogPostRepository.findApprovedPostsByTagOrTitle(searchPhrase, page, pageSize).stream()
        .map(this::fromBlogPost)
        .collect(Array.collector());
  }

  public long countByTitleOrTag(String searchPhrase) {
    return blogPostRepository.countApprovedPostsByTagOrTitle(searchPhrase);
  }

  private SearchedBlogPostForListing fromBlogPost(BlogPost blogPost) {
    return SearchedBlogPostForListing.builder()
        .title(blogPost.getTitle())
        .url(blogPost.getUrl())
        .publishedTime(blogPost.getPublishedDate())
        .author(blogPost.getBlog().getAuthor())
        .authorTwitterHandle(blogPost.getBlog().getTwitter())
        .build();
  }

}
