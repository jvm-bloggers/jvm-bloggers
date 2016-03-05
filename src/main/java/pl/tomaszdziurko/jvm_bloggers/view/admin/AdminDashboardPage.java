package pl.tomaszdziurko.jvm_bloggers.view.admin;

import java.time.LocalDateTime;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

@AuthorizeInstantiation(Roles.ADMIN)
public class AdminDashboardPage extends AbstractAdminPage {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    @SpringBean
    private NowProvider nowProvider;

    public AdminDashboardPage() {
        LocalDateTime lastPublishedDate = DateTimeUtilities.lastMailingDate(nowProvider);
        add(new Label("newPostsCounter", new AbstractReadOnlyModel<Integer>() {
            @Override
            public Integer getObject() {
                return blogPostRepository.countByPublishedDateAfter(lastPublishedDate);
            }
        }));
    }
}
