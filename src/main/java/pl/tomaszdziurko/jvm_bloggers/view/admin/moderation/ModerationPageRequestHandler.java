package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;

import java.util.Iterator;

@Component
@Slf4j
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ModerationPageRequestHandler implements IDataProvider<BlogPost> {

    private final BlogPostRepository blogPostRepository;

    @Value("${items.pagination.size}")
    private int paginationLimit;

    @Override
    public Iterator<? extends BlogPost> iterator(long first, long count) {
        log.debug("Refreshing data, first {}, count {}", first, count);
        int page = Long.valueOf(first / paginationLimit).intValue();
        long start = System.currentTimeMillis();
        Iterator<BlogPost> iterator = blogPostRepository
            .findLatestPosts(new PageRequest(page, paginationLimit)).iterator();
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
