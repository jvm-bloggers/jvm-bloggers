package com.jvm_bloggers.core.data_fetching.http;

import com.google.common.annotations.VisibleForTesting;

import lombok.AccessLevel;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import net.jcip.annotations.ThreadSafe;

import org.apache.commons.collections4.MapUtils;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
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
@Component
@ThreadSafe
@Slf4j
@RequiredArgsConstructor(access = AccessLevel.PACKAGE)
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
                                    Map<String, String> headers) throws IOException {

        HttpURLConnection conn = (HttpURLConnection) urlConnection;
        int redirectCounter = 0;

        do {
            setupConnection(conn, headers);

            final int responseCode = conn.getResponseCode();
            // handle redirect between different protocols
            switch (responseCode) {
                case HttpURLConnection.HTTP_MOVED_PERM:
                case HttpURLConnection.HTTP_MOVED_TEMP:
                    conn = handleRedirect(conn);
                    redirectCounter = incrementRedirectCounterOrThrow(
                        (HttpURLConnection) urlConnection,
                        redirectCounter);
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

    private void setupConnection(HttpURLConnection conn, Map<String, String> headers) {
        conn.setConnectTimeout(DEFAULT_TIMEOUT);
        conn.setReadTimeout(DEFAULT_TIMEOUT);
        // handle redirects within the same protocol
        conn.setInstanceFollowRedirects(true);
        if (MapUtils.isNotEmpty(headers)) {
            setupHeaders(conn, headers);
        }
    }

    private void setupHeaders(HttpURLConnection conn, Map<String, String> headers) {
        headers.forEach(conn::setRequestProperty);
    }

    private HttpURLConnection handleRedirect(HttpURLConnection conn) throws IOException {
        log.debug("Handling redirect for url: {} ", conn.getURL());
        conn.disconnect();

        final String location = conn.getHeaderField(LOCATION_HEADER);
        final URL redirectUrl = new URL(location);

        log.debug("Redirect URL: {}", redirectUrl);
        return (HttpURLConnection) redirectUrl.openConnection();
    }

    private static final class TooManyRedirectsException extends IOException {

        private TooManyRedirectsException(URL url) {
            super("Too many redirects occurred trying to open url: " + url);
        }
    }

}
