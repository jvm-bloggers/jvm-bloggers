package com.jvm_bloggers.admin_panel.blogs;

import com.jvm_bloggers.admin_panel.PaginationConfiguration;
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost;
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository;

import lombok.RequiredArgsConstructor;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.springframework.data.domain.PageRequest;

import java.util.Iterator;
import java.util.Optional;

@RequiredArgsConstructor
public class BlogPostsPageRequestHandler implements IDataProvider<BlogPost> {

    private final PaginationConfiguration paginationConfiguration;
    private final BlogPostRepository blogPostRepository;
    private final BlogRepository blogRepository;
    private final Long blogId;

    @Override
    public Iterator<? extends BlogPost> iterator(long first, long count) {
        int page = Long.valueOf(first / paginationConfiguration.getDefaultPageSize()).intValue();
        return blogPostRepository
            .findByBlogIdOrderByPublishedDateDesc(blogId, new PageRequest(page,
                paginationConfiguration.getDefaultPageSize())
            ).iterator();
    }

    @Override
    public long size() {
        return blogPostRepository.countByBlogId(blogId);
    }

    @Override
    public IModel<BlogPost> model(BlogPost blog) {
        return new BlogPostModel(blog);
    }

    @Override
    public void detach() {

    }

    String getPageHeader() {
        return Optional.ofNullable(blogRepository.findOne(blogId))
            .map(b -> b.getAuthor() + "'s posts")
            .orElse("No such blog found");
    }
}
