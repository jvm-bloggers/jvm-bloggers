package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.DEV;
import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.TEST;

/**
 * @author Adam Dec
 */
@Component
@Profile({DEV, TEST})
public class DummyBlogPostsFetcher implements BlogPostsFetcher {

    private volatile boolean refreshed;

    @Override
    public void refreshPosts() {
        refreshed = true;
    }

    public boolean isRefreshed() {
        return refreshed;
    }
}
