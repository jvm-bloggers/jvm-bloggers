package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;

import static org.apache.commons.lang3.StringUtils.abbreviate;
import static pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;
import static pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities.lastPublicationDate;

/**
 * @author mszarlinski
 */
@Component
public class BlogPostItemPopulator {

    private static final String ACTION_PANEL_WICKET_ID = "actionPanel";

    @Autowired
    private NowProvider nowProvider;

    public void populateItem(final Item<BlogPost> item, final Form<Void> moderationForm,
                             final CustomFeedbackPanel feedbackPanel) {
        final BlogPost post = item.getModelObject();
        item.add(new Label("title", post.getTitle()));
        item.add(new Label("author", post.getBlog().getAuthor()));
        item.add(new ExternalLink("link", post.getUrl(), abbreviate(post.getUrl(), 90)));
        item.add(new Label("date", post.getPublishedDate().format(DATE_FORMATTER)));
        item.add(new Label("approved", post.getApprovalState()));
        addEvenOddRowStyling(item);

        final boolean postIsGoingInNewsletter = post.isGoingInNewsletter(
                lastPublicationDate(nowProvider.now()));

        item.add(new ModerationActionPanel(ACTION_PANEL_WICKET_ID, moderationForm,
                feedbackPanel, item.getModel(), !postIsGoingInNewsletter));

        if (postIsGoingInNewsletter) {
            addHighlightedRowStyling(item);
        }
    }

    private void addEvenOddRowStyling(final Item<BlogPost> item) {
        item.add(AttributeModifier.append("class",
                (item.getIndex() % 2 == 1) ? "even" : "odd"));
    }

    private void addHighlightedRowStyling(final Item<BlogPost> item) {
        item.add(AttributeModifier.append("class", "highlighted-post"));
    }
}
