package com.jvm_bloggers.frontend.admin_area.moderation;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import com.jvm_bloggers.utils.NowProvider;

import lombok.RequiredArgsConstructor;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER;
import static com.jvm_bloggers.utils.DateTimeUtilities.lastPublicationDate;
import static org.apache.commons.lang3.StringUtils.abbreviate;

/**
 * @author mszarlinski
 */
@Component
@RequiredArgsConstructor
public class BlogPostItemPopulator {

    private static final String ACTION_PANEL_WICKET_ID = "actionPanel";

    private final NowProvider nowProvider;

    public void populateItem(final Item<BlogPost> item, final Form<Void> moderationForm,
                             final CustomFeedbackPanel feedbackPanel) {
        final BlogPost post = item.getModelObject();
        item.add(new Label("title", post.getTitle()));
        item.add(new Label("author", post.getBlog().getAuthor()));
        item.add(new ExternalLink("link", post.getUrl(), abbreviate(post.getUrl(), 90)));
        item.add(new Label("date", post.getPublishedDate().format(DATE_TIME_FORMATTER)));
        item.add(new Label("approved", post.getApprovalState()));
        addEvenOddRowStyling(item);

        final boolean postIsGoingInNewsletter = post.isGoingInNewsletter(
                lastPublicationDate(nowProvider.now()));

        item.add(new ModerationActionPanel(ACTION_PANEL_WICKET_ID, moderationForm,
                feedbackPanel, item.getModel()));

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
