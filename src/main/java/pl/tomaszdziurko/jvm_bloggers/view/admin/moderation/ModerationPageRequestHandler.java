package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;

import java.util.Iterator;

@Component
@Slf4j
public class ModerationPageRequestHandler implements IDataProvider<BlogPost> {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    public ModerationPageRequestHandler() {
    }

    @Autowired
    public ModerationPageRequestHandler(BlogPostRepository blogPostRepository) {
        this.blogPostRepository = blogPostRepository;
    }

    @Override
    public Iterator<? extends BlogPost> iterator(long first, long count) {
        log.debug("Refreshing data, first {}, count {}", first, count);
        int page = Long.valueOf(first / ModerationPage.BLOG_POSTS_PER_PAGE).intValue();
        int countInt = Long.valueOf(count).intValue();
        long start = System.currentTimeMillis();
        Iterator<BlogPost> iterator = blogPostRepository
            .findLatestPosts(new PageRequest(page, countInt)).iterator();
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
