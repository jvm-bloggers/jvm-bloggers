package com.jvm_bloggers.frontend.public_area;

import com.googlecode.wicket.jquery.ui.markup.html.link.BookmarkablePageLink;
import com.jvm_bloggers.frontend.public_area.top_posts.TopPostsPage;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("fire-in-ovh")
public class FireInOvhPage extends AbstractFrontendPage {

    public FireInOvhPage() {
    }

    @Override
    protected String getPageTitle() {
        return "Pożar w OVH i utrata danych";
    }

}
