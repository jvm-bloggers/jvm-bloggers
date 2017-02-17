package com.jvm_bloggers.frontend.admin_area.counters;

import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository;
import com.jvm_bloggers.utils.DateTimeUtilities;
import com.jvm_bloggers.utils.NowProvider;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.time.LocalDateTime;

public class NewPostsCounterModel extends AbstractReadOnlyModel<Integer> {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    @SpringBean
    private NowProvider nowProvider;

    public NewPostsCounterModel() {
        Injector.get().inject(this);
    }

    @Override
    public Integer getObject() {
        LocalDateTime lastPublicationDate =
            DateTimeUtilities.lastPublicationDate(nowProvider.now());
        return blogPostRepository.countByPublishedDateAfter(lastPublicationDate);
    }
}
