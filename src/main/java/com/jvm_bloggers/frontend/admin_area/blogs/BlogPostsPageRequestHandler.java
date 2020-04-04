package com.jvm_bloggers.frontend.admin_area.blogs;

import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;

import io.vavr.control.Option;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.springframework.data.domain.PageRequest;

import java.util.Iterator;

@Slf4j
@RequiredArgsConstructor
public class BlogPostsPageRequestHandler implements IDataProvider<BlogPost> {

    private final PaginationConfiguration paginationConfiguration;

    private final BlogPostRepository blogPostRepository;

    private final BlogRepository blogRepository;

    private final Long blogId;

    @Override
    public Iterator<? extends BlogPost> iterator(long first, long count) {
        log.debug("Refreshing data, first {}, count {}", first, count);
        int page = (int) (first / paginationConfiguration.getDefaultPageSize());
        long start = System.currentTimeMillis();
        Iterator<BlogPost> iterator = blogPostRepository
            .findByBlogIdOrderByPublishedDateDesc(
              blogId,
              PageRequest.of(page, paginationConfiguration.getDefaultPageSize())
            ).iterator();
        long stop = System.currentTimeMillis();
        log.debug("Iterator() execution time = " + (stop - start) + " ms");
        return iterator;
    }

    @Override
    public long size() {
        long start = System.currentTimeMillis();
        long count = blogPostRepository.countByBlogId(blogId);
        long stop = System.currentTimeMillis();
        log.debug("Size() execution time = " + (stop - start) + " ms");
        return count;
    }

    @Override
    public IModel<BlogPost> model(BlogPost blog) {
        return new BlogPostModel(blog);
    }

    @Override
    public void detach() {

    }

    String getPageHeader() {
        return Option.ofOptional(blogRepository.findById(blogId))
            .map(b -> b.getAuthor() + "'s posts")
            .getOrElse("No such blog found");
    }
}
