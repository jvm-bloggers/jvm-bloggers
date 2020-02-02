package com.jvm_bloggers.frontend.admin_area.blogs;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

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
        return blogPostRepository
          .findById(blogPostId)
          .orElseThrow(() -> new RuntimeException("Blog post with id " + blogPostId + " not found"));
    }

}
