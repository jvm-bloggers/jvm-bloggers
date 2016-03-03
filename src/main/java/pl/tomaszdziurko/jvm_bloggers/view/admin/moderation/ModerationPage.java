package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AbstractAdminPage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomPagingNavigator;

import java.time.format.DateTimeFormatter;

@AuthorizeInstantiation(Roles.ADMIN)
public class ModerationPage extends AbstractAdminPage {

    public static final long BLOG_POSTS_PER_PAGE = 15;

    public static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm");

    @SpringBean
    private ModerationPageRequestHandler requestHandler;

    private final CustomFeedbackPanel feedback;

    public ModerationPage() {
        feedback = new CustomFeedbackPanel("feedback");
        add(feedback);

        Form<Void> moderationForm = new Form<>("moderationForm");
        moderationForm.setOutputMarkupId(true);
        add(moderationForm);
        DataView<BlogPost> dataView = new DataView<BlogPost>("pageable", requestHandler) {

            @Override
            protected void populateItem(final Item<BlogPost> item) {
                BlogPost post = item.getModelObject();
                item.add(new Label("title", post.getTitle()));
                item.add(new Label("author", post.getBlog().getAuthor()));
                item.add(new ExternalLink("link", post.getUrl(), StringUtils.abbreviate(post.getUrl(), 90)));
                item.add(new Label("date", post.getPublishedDate().format(DATE_FORMATTER)));
                item.add(new Label("approved", post.getApprovalState()));
                item.add(new ModerationActionPanel("actionPanel", moderationForm, feedback, item.getModel()));
                addEvenOddRowStyling(item);
            }
        };

        dataView.setItemsPerPage(BLOG_POSTS_PER_PAGE);
        moderationForm.add(dataView);
        moderationForm.add(new CustomPagingNavigator("navigator", dataView));
    }

    private void addEvenOddRowStyling(final Item<BlogPost> item) {
        item.add(AttributeModifier.replace("class", new AbstractReadOnlyModel<String>() {
            @Override
            public String getObject() {
                return (item.getIndex() % 2 == 1) ? "even" : "odd";
            }
        }));
    }

}
