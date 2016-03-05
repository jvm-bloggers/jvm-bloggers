package pl.tomaszdziurko.jvm_bloggers.view.admin;

import java.time.LocalDateTime;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

/**
 * @author Mateusz Urbański <matek2305@gmail.com>
 */
public class DashboardNewPostsCounterModel extends AbstractReadOnlyModel<Integer> {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    @SpringBean
    private NowProvider nowProvider;

    public DashboardNewPostsCounterModel() {
        Injector.get().inject(this);
    }

    @Override
    public Integer getObject() {
        LocalDateTime lastPublicationDate = DateTimeUtilities.lastPublicationDate(nowProvider);
        return blogPostRepository.countByPublishedDateAfter(lastPublicationDate);
    }
}
