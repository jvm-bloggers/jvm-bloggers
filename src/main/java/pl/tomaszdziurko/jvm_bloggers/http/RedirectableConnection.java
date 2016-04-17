package pl.tomaszdziurko.jvm_bloggers.http;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Map;

public class RedirectableConnection implements Closeable {

    private static final String FAKE_USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) "
        + "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.1 Safari/537.36";

    private final ProtocolSwitchingAwareConnectionRedirectHandler redirectHandler =
        new ProtocolSwitchingAwareConnectionRedirectHandler();

    private final Map<String, List<String>> headers =
        ImmutableMap.of("User-Agent", ImmutableList.of(FAKE_USER_AGENT));
    private final URLConnection urlConnection;

    public RedirectableConnection(String url) throws IOException {
        URLConnection connection = new URL(url).openConnection();
        this.urlConnection = redirectHandler.handle(connection, headers);
    }

    public InputStream getInputStream() throws IOException {
        return urlConnection.getInputStream();
    }

    @Override
    public void close() {
        IOUtils.close(urlConnection);
    }
}
