package com.jvm_bloggers.domain.query.blog_post_for_listing;

import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;

import io.vavr.collection.List;
import io.vavr.control.Option;
import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class BlogPostForListingQuery {

    private final BlogRepository blogRepository;
    private final BlogPostRepository blogPostRepository;

    public long countByBlogId(long blogId) {
        return blogPostRepository.countByBlogId(blogId);
    }

    public Option<Long> findBlogIdByBookmarkableId(String bookmarkableId) {
        return blogRepository.findByBookmarkableId(bookmarkableId)
            .map(Blog::getId);
    }

    public Option<BlogDisplayDetails> findBlogDisplayDetails(Long blogId) {
        return Option.ofOptional(blogRepository.findById(blogId))
            .map(BlogDisplayDetails::fromBlog);
    }

    public List<BlogPostForListing> findBlogPosts(Long blogId, int page, int size) {
        return List
            .ofAll(
                blogPostRepository.findByBlogIdAndApprovedTrueOrderByPublishedDateDesc(
                    blogId,
                    PageRequest.of(page, size)
                )
            )
            .map(BlogPostForListing::fromBlogPost);
    }
}
