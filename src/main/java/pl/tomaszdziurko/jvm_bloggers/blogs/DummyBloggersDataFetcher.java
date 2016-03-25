package pl.tomaszdziurko.jvm_bloggers.blogs;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.DEV;
import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.TEST;

/**
 * @author Adam Dec
 */
@Component
@Profile({TEST})
public class DummyBloggersDataFetcher implements BloggersDataFetcher {

    private volatile boolean refreshed;

    @Override
    public void refreshData() {
        refreshed = true;
    }

    public boolean isRefreshed() {
        return refreshed;
    }
}
