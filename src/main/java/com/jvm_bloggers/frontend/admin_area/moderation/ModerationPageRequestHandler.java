package com.jvm_bloggers.frontend.admin_area.moderation;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;
import com.jvm_bloggers.frontend.admin_area.blogs.BlogPostModel;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import java.util.Iterator;

@Component
@Slf4j
@NoArgsConstructor
@AllArgsConstructor(onConstructor_ = {@Autowired})
public class ModerationPageRequestHandler implements IDataProvider<BlogPost> {

    private BlogPostRepository blogPostRepository;

    private PaginationConfiguration paginationConfiguration;

    @Override
    public Iterator<? extends BlogPost> iterator(long first, long count) {
        log.debug("Refreshing data, first {}, count {}", first, count);
        int page = (int) (first / paginationConfiguration.getDefaultPageSize());
        long start = System.currentTimeMillis();
        Iterator<BlogPost> iterator = blogPostRepository
            .findLatestPosts(PageRequest.of(page, paginationConfiguration.getDefaultPageSize()))
            .iterator();
        long stop = System.currentTimeMillis();
        log.debug("Iterator() execution time = " + (stop - start) + " ms");
        return iterator;
    }

    @Override
    public long size() {
        long start = System.currentTimeMillis();
        long count = blogPostRepository.count();
        long stop = System.currentTimeMillis();
        log.debug("Size() execution time = " + (stop - start) + " ms");
        return count;
    }

    @Override
    public IModel<BlogPost> model(BlogPost object) {
        return new BlogPostModel(object);
    }

    @Override
    public void detach() {

    }
}
