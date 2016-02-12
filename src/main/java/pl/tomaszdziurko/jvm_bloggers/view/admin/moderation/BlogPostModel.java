package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;

@Slf4j
public class BlogPostModel extends LoadableDetachableModel<BlogPost> {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    private Long blogPostId;

    public BlogPostModel(BlogPost blogPost) {
        super(blogPost);
        Injector.get().inject(this);
        blogPostId = blogPost.getId();
    }

    @Override
    protected BlogPost load() {
        log.debug("Loading post with id " + blogPostId);
        return blogPostRepository.findOne(blogPostId);
    }
}
