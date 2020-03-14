package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.entities.blog.BlogType;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath(value = "/podcasts")
public class PodcastBlogsPage extends AbstractBlogsPage {

    @Override
    protected String getPageTitle() {
        return "Podcasty";
    }

    @Override
    protected Class<? extends AbstractBlogsPage> getActiveClass() {
        return this.getClass();
    }

    @Override
    protected BlogType getBlogType() {
        return BlogType.PODCAST;
    }
}
