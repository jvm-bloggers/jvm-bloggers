package com.jvm_bloggers.core.rss;

import com.google.common.collect.ImmutableMap;
import com.jvm_bloggers.core.data_fetching.http.ProtocolSwitchingAwareConnectionRedirectHandler;
import com.jvm_bloggers.core.utils.Validators;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;
import com.rometools.rome.io.XmlReader;
import javaslang.control.Option;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.springframework.stereotype.Component;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.UUID;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipException;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

@Component
@Slf4j
public class SyndFeedProducer {

    private static final String FAKE_USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) "
        + "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.1 Safari/537.36";

    private final ProtocolSwitchingAwareConnectionRedirectHandler redirectHandler =
        new ProtocolSwitchingAwareConnectionRedirectHandler();

    /**
     * Try to fetch rss from given url.
     * 1st attempt - standard https connection
     * 2nd attempt - fallback for some self-signed certificates using https connection with disabled SSL cerficiate checking
     * 3rd attempt - execute wget, save as a file in tmp directory and create SyndFeed from this file
     */
    public Option<SyndFeed> createFor(String rssUrl) {

        return createInADefaultWay(rssUrl)
            .orElse(() -> createWithoutSslVerification(rssUrl))
            .orElse(() -> createWithWget(rssUrl));
    }

    public Option<String> validUrlFromRss(String rss) {
        Option<String> url = createFor(rss).map(SyndFeed::getLink);
        return url.filter(Validators::isUrlValid);
    }

    private Option<SyndFeed> createInADefaultWay(String rssUrl) {
        URLConnection urlConnection = null;
        InputStream inputStream = null;
        try {
            urlConnection = new URL(rssUrl).openConnection();
            final Map<String, String> headers =
                    ImmutableMap.of("User-Agent", FAKE_USER_AGENT);
            urlConnection = redirectHandler.handle(urlConnection, headers);

            inputStream = wrapToGzipStreamIfNeeded(urlConnection.getInputStream());
            return Option.of(new SyndFeedInput().build(new XmlReader(inputStream)));
        } catch (Exception ex) {
            log.warn("Error during fetching RSS {} url: {}", rssUrl, ex.getMessage());
            return Option.none();
        } finally {
            IOUtils.closeQuietly(inputStream);
            IOUtils.close(urlConnection);
        }
    }

    private Option<SyndFeed> createWithoutSslVerification(String rssUrl) {
        URLConnection urlConnection = null;
        InputStream inputStream = null;
        try {
            urlConnection = new URL(rssUrl).openConnection();
            final Map<String, String> headers =
                    ImmutableMap.of("User-Agent", FAKE_USER_AGENT);
            urlConnection = redirectHandler.handle(urlConnection, headers);
            configureHttpsConnectionToTrustAnyone(urlConnection);
            inputStream = wrapToGzipStreamIfNeeded(urlConnection.getInputStream());
            return Option.of(new SyndFeedInput().build(new XmlReader(inputStream)));
        } catch (Exception ex) {
            log.warn(
                "Error during fetching RSS without https check for {} url: {}",
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

            TrustManager[] trustAllCerts = new TrustManager[] { new X509TrustManager() {
                public X509Certificate[] getAcceptedIssuers() {
                    return null;
                }

                public void checkClientTrusted(X509Certificate[] certs, String authType) {
                }

                public void checkServerTrusted(X509Certificate[] certs, String authType) {
                }
            } };
            SSLContext sc = SSLContext.getInstance("TLSv1.2");
            sc.init(null, trustAllCerts, new java.security.SecureRandom());
            httpsConnection.setSSLSocketFactory(sc.getSocketFactory());
            httpsConnection.setHostnameVerifier((hostname, session) -> true);
        }
    }

    private static Option<SyndFeed> createWithWget(String rssUrl) {
        String uid = UUID.randomUUID().toString();
        try {
            Process process = new ProcessBuilder("wget", rssUrl, "-O", "/tmp/" + uid).start();
            process.waitFor();
            File file = new File("/tmp/" + uid);
            Option<SyndFeed> of = Option.of(new SyndFeedInput().build(file));
            file.delete();
            return of;
        } catch (Exception exc) {
            log.warn("Exception during wget execution for url {}: {}", rssUrl, exc.getMessage());
            return Option.none();
        }
    }

    private InputStream wrapToGzipStreamIfNeeded(InputStream inputStream) throws IOException {
        if (!inputStream.markSupported()) {
            inputStream = new BufferedInputStream(inputStream);
        }
        inputStream.mark(1000);
        try {
            return new GZIPInputStream(inputStream);
        } catch (ZipException ex) {
            inputStream.reset();
            return inputStream;
        }
    }
}
