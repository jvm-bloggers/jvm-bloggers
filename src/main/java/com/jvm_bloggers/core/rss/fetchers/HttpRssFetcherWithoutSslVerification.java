package com.jvm_bloggers.core.rss.fetchers;

import com.google.common.collect.ImmutableMap;
import com.jvm_bloggers.core.data_fetching.http.GzipStreamWrapper;
import com.jvm_bloggers.core.data_fetching.http.ProtocolSwitchingAwareConnectionRedirectHandler;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;
import com.rometools.rome.io.XmlReader;

import io.vavr.control.Try;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.core.annotation.Order;
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

import static org.apache.commons.io.IOUtils.close;
import static org.apache.commons.io.IOUtils.closeQuietly;

/**
 * Fallback for some self-signed certificates using https connection with
 * disabled SSL certificate checking
 */
@Slf4j
@Order(0)
@Component
@RequiredArgsConstructor
public class HttpRssFetcherWithoutSslVerification implements RssFetcher {

    private final ProtocolSwitchingAwareConnectionRedirectHandler redirectHandler;
    private final GzipStreamWrapper gzipStreamWrapper;

    @Override
    public Try<SyndFeed> fetch(String rssUrl) {
        URLConnection urlConnection = null;
        InputStream inputStream = null;
        try {
            urlConnection = new URL(rssUrl).openConnection();
            final Map<String, String> headers =
                ImmutableMap.of("User-Agent", FAKE_USER_AGENT);
            urlConnection = redirectHandler.handle(urlConnection, headers);
            configureHttpsConnectionToTrustAnyone(urlConnection);
            inputStream = gzipStreamWrapper.wrap(urlConnection.getInputStream());
            return Try.success(new SyndFeedInput().build(new XmlReader(inputStream)));
        } catch (Exception ex) {
            log.info("Problem during fetching RSS without https check for {} url: {}",
                rssUrl,
                ex.getMessage()
            );
            return Try.failure(ex);
        } finally {
            closeQuietly(inputStream);
            close(urlConnection);
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
