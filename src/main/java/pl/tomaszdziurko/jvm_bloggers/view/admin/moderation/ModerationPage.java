package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import com.googlecode.wicket.jquery.ui.markup.html.link.AjaxLink;

import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.BlogPostsFetcher;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.BloggersDataFetcher;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AbstractAdminPage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomPagingNavigator;

import java.time.format.DateTimeFormatter;

@Slf4j
@AuthorizeInstantiation(Roles.ADMIN)
public class ModerationPage extends AbstractAdminPage {

    public static final long BLOG_POSTS_PER_PAGE = 15;
    public static final String FETCH_NEW_BLOGS_BUTTON_ID = "fetchNewBlogsButton";
    public static final String FETCH_NEW_POSTS_BUTTON_ID = "fetchNewPostsButton";
    public static final String MODERATION_FORM_ID = "moderationForm";

    public static final DateTimeFormatter DATE_FORMATTER =
        DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm");

    @SpringBean
    private ModerationPageRequestHandler requestHandler;

    @SpringBean
    private BloggersDataFetcher bloggersDataFetcher;

    @SpringBean
    private BlogPostsFetcher blogPostsFetcher;

    private final CustomFeedbackPanel feedback;

    public ModerationPage() {
        feedback = new CustomFeedbackPanel("feedback");
        add(feedback);

        final Form<Void> moderationForm = new Form<>(MODERATION_FORM_ID);
        moderationForm.setOutputMarkupId(true);
        add(moderationForm);
        final DataView<BlogPost> dataView = new DataView<BlogPost>("pageable", requestHandler) {

            @Override
            protected void populateItem(final Item<BlogPost> item) {
                BlogPost post = item.getModelObject();
                item.add(new Label("title", post.getTitle()));
                item.add(new Label("author", post.getBlog().getAuthor()));
                item.add(new ExternalLink("link",
                    post.getUrl(), StringUtils.abbreviate(post.getUrl(), 90)));
                item.add(new Label("date", post.getPublishedDate().format(DATE_FORMATTER)));
                item.add(new Label("approved", post.getApprovalState()));
                item.add(new ModerationActionPanel("actionPanel", moderationForm,
                    feedback, item.getModel()));
                addEvenOddRowStyling(item);
            }
        };
        dataView.setItemsPerPage(BLOG_POSTS_PER_PAGE);

        add(new AjaxLink(FETCH_NEW_BLOGS_BUTTON_ID) {
            @Override
            public void onClick(AjaxRequestTarget target) {
                bloggersDataFetcher.refreshData();
                log.info("Refreshed data for new blogs");
            }
        });

        add(new AjaxLink(FETCH_NEW_POSTS_BUTTON_ID) {
            @Override
            public void onClick(AjaxRequestTarget target) {
                blogPostsFetcher.refreshPosts();
                log.info("Refreshed data for new posts");
            }
        });

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
