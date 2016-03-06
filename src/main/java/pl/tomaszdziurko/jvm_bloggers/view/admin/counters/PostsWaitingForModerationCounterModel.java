package pl.tomaszdziurko.jvm_bloggers.view.admin.counters;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;

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
