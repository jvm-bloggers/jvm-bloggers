package com.jvm_bloggers.frontend.admin_area.counters;

import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class PostsWaitingForModerationCounterModel extends AbstractReadOnlyModel<Integer> {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    public PostsWaitingForModerationCounterModel() {
        Injector.get().inject(this);
    }

    @Override
    public Integer getObject() {
        return blogPostRepository.countByApprovedIsNull();
    }
}
