package pl.tomaszdziurko.jvm_bloggers.http;

import com.google.common.annotations.VisibleForTesting;

import lombok.AccessLevel;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import net.jcip.annotations.ThreadSafe;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Map;


/**
 * Handles the case where {@link java.net.HttpURLConnection} gets redirected by
 * <tt>301</tt> or <tt>302</tt> HTTP response code and the redirect involves
 * switching between protocols. From security considerations
 * {@link java.net.HttpURLConnection} redirection logic does not allow for such
 * cases but only for redirection within the same protocol. This handler is a
 * custom workaround for that case.
 *
 * @author Marcin Kłopotek
 */
@ThreadSafe
@Slf4j
@RequiredArgsConstructor(access = AccessLevel.PACKAGE, onConstructor = @__(@VisibleForTesting))
public class ProtocolSwitchingAwareConnectionRedirectHandler {

    @VisibleForTesting
    static final String LOCATION_HEADER = "Location";
    @VisibleForTesting
    static final int DEFAULT_TIMEOUT = 30_000;

    private static final int REDIRECT_LIMIT = 20;
    private final int redirectLimit;

    public ProtocolSwitchingAwareConnectionRedirectHandler() {
        this(REDIRECT_LIMIT);
    }

    /**
     * Tries to handle HTTP redirect and returns connection to the redirect location
     * or an <code>urlConnection</code> itself if there was no redirect required.
     *
     * @param urlConnection original connection
     * @param headers       optional HTTP request headers map (can be <tt>null</tt>)
     * @return HTTP connection to redirect target or connection passed as a parameter
     *     if there was no redirect (<tt>HTTP 200 OK</tt>)
     * @throws IOException               upon underlying communication failure
     * @throws TooManyRedirectsException if redirect hops exceeds
     *                                   {@link #REDIRECT_LIMIT redirects limit}
     */
    public HttpURLConnection handle(@NonNull URLConnection urlConnection,
                                    Map<String, List<String>> headers) throws IOException {

        HttpURLConnection conn = (HttpURLConnection) urlConnection;
        int redirectCounter = 0;

        do {
            setupConnection(conn, headers);

            final int responseCode = conn.getResponseCode();
            // handle redirect between different protocols
            switch (responseCode) {
                case HttpURLConnection.HTTP_MOVED_PERM:
                case HttpURLConnection.HTTP_MOVED_TEMP:
                    conn = handleRedirect(conn, headers);
                    redirectCounter = incrementRedirectCounterOrThrow(conn, redirectCounter);
                    continue;
                default:
                    return conn;
            }

        } while (true);

    }

    private int incrementRedirectCounterOrThrow(HttpURLConnection httpConnection,
                                                int redirectCounter)
        throws TooManyRedirectsException {
        if (++redirectCounter > redirectLimit) {
            throw new TooManyRedirectsException(httpConnection.getURL());
        }
        return redirectCounter;
    }

    private void setupConnection(HttpURLConnection conn, Map<String, List<String>> headers) {
        conn.setConnectTimeout(DEFAULT_TIMEOUT);
        conn.setReadTimeout(DEFAULT_TIMEOUT);
        // handle redirects within the same protocol
        conn.setInstanceFollowRedirects(true);
        if (headers != null) {
            setupHeaders(conn, headers);
        }
    }

    private void setupHeaders(HttpURLConnection conn, Map<String, List<String>> headers) {
        headers.entrySet().forEach(header -> {
            header.getValue().forEach(value -> {
                conn.setRequestProperty(header.getKey(), value);
            });
        });
    }

    private HttpURLConnection handleRedirect(HttpURLConnection conn,
                                             Map<String, List<String>> headers) throws IOException {
        log.debug("Handling redirect for url: {} ", conn.getURL());
        conn.disconnect();

        final String location = conn.getHeaderField(LOCATION_HEADER);
        final URL redirectUrl = new URL(location);

        log.debug("Redirect URL: {}", redirectUrl);
        HttpURLConnection redirectedConnection = (HttpURLConnection) redirectUrl.openConnection();
        setupConnection(redirectedConnection, headers);
        return redirectedConnection;
    }

    private static final class TooManyRedirectsException extends IOException {

        private TooManyRedirectsException(URL url) {
            super("Too many redirects occurred trying to open url: " + url);
        }
    }

}
