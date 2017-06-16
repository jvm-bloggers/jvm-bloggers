package com.jvm_bloggers.frontend.public_area;

import com.googlecode.wicket.jquery.ui.markup.html.link.BookmarkablePageLink;
import com.jvm_bloggers.frontend.public_area.top_posts.TopPostsPage;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("about")
public class AboutPage  extends AbstractFrontendPage {

    public AboutPage() {
        add(new BookmarkablePageLink<TopPostsPage>("topPostsLink", TopPostsPage.class));
    }

    @Override
    protected String getPageTitle() {
        return "O projekcie";
    }

}
