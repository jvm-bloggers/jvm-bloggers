package com.jvm_bloggers.frontend.public_area.blogs.single_blog;

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogDisplayDetails;
import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogPostForListing;
import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogPostForListingQuery;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;

import io.vavr.control.Option;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class BlogPostsPageBackingBean {

    private final BlogPostForListingQuery blogPostForListingQuery;

    private final LinkGenerator linkGenerator;

    private final PaginationConfiguration paginationConfiguration;

    public int defaultPageSize() {
        return paginationConfiguration.getDefaultPageSize();
    }

    public Option<BlogDisplayDetails> findBlogDisplayDetails(Long blogId) {
        return blogPostForListingQuery.findBlogDisplayDetails(blogId);
    }

    public String generateRedirectLink(BlogPostForListing blogPostForListing) {
        return linkGenerator.generateRedirectLinkFor(blogPostForListing.getUid());
    }

    public BlogPostsPageRequestHandler requestHandler(String bookmarkableId) {
        return new BlogPostsPageRequestHandler(bookmarkableId);
    }
}
