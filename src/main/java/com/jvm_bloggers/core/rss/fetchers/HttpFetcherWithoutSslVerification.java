package com.jvm_bloggers.core.rss.fetchers;

import com.google.common.collect.ImmutableMap;
import com.jvm_bloggers.core.data_fetching.http.GzipStreamWrapper;
import com.jvm_bloggers.core.data_fetching.http.ProtocolSwitchingAwareConnectionRedirectHandler;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;
import com.rometools.rome.io.XmlReader;

import javaslang.control.Option;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.io.IOUtils;
import org.springframework.stereotype.Component;

import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Map;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

@Slf4j
@Component
@RequiredArgsConstructor
public class HttpFetcherWithoutSslVerification implements Fetcher {

    private static final String FAKE_USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) "
        + "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.1 Safari/537.36";

    private final ProtocolSwitchingAwareConnectionRedirectHandler redirectHandler;
    private final GzipStreamWrapper gzipStreamWrapper;

    @Override
    public Option<SyndFeed> fetch(String rssUrl) {
        URLConnection urlConnection = null;
        InputStream inputStream = null;
        try {
            urlConnection = new URL(rssUrl).openConnection();
            final Map<String, String> headers =
                ImmutableMap.of("User-Agent", FAKE_USER_AGENT);
            urlConnection = redirectHandler.handle(urlConnection, headers);
            configureHttpsConnectionToTrustAnyone(urlConnection);
            inputStream = gzipStreamWrapper.wrap(urlConnection.getInputStream());
            return Option.of(new SyndFeedInput().build(new XmlReader(inputStream)));
        } catch (Exception ex) {
            log.warn("Error during fetching RSS without https check for {} url: {}",
                rssUrl,
                ex.getMessage()
            );
            return Option.none();
        } finally {
            IOUtils.closeQuietly(inputStream);
            IOUtils.close(urlConnection);
        }
    }

    private void configureHttpsConnectionToTrustAnyone(URLConnection urlConnection)
        throws NoSuchAlgorithmException, KeyManagementException {
        if (urlConnection instanceof HttpsURLConnection) {
            HttpsURLConnection httpsConnection = (HttpsURLConnection) urlConnection;

            TrustManager[] trustAllCerts = new TrustManager[]{
                new X509TrustManager() {
                    public X509Certificate[] getAcceptedIssuers() {
                        return null;
                    }

                    public void checkClientTrusted(X509Certificate[] certs, String authType) {
                    }

                    public void checkServerTrusted(X509Certificate[] certs, String authType) {
                    }
                }
            };
            SSLContext sc = SSLContext.getInstance("TLSv1.2");
            sc.init(null, trustAllCerts, new java.security.SecureRandom());
            httpsConnection.setSSLSocketFactory(sc.getSocketFactory());
            httpsConnection.setHostnameVerifier((hostname, session) -> true);
        }
    }

}
