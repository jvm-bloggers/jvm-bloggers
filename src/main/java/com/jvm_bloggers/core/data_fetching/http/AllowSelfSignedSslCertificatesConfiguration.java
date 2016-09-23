package com.jvm_bloggers.core.data_fetching.http;

import com.google.common.collect.ObjectArrays;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import org.postgresql.ssl.NonValidatingFactory;
import org.springframework.stereotype.Component;

import java.security.GeneralSecurityException;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;

import javax.annotation.PostConstruct;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

/**
 * The configuration allows self signed SSL certificates for HTTPS connections.
 * It should be disabled if application security is important. In such case
 * appropriate client SSL certificates are required to be added to the local
 * key store in order to perform correct SSL handshake.
 *
 * @author Marcin Kłopotek
 */
@Component
@Slf4j
public class AllowSelfSignedSslCertificatesConfiguration {

    @SneakyThrows(GeneralSecurityException.class)
    private static void disableSslVerification() {
        final TrustManager[] trustAllCerts = new TrustManager[]{new X509TrustManager() {
            @Override
            public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                return ObjectArrays.newArray(java.security.cert.X509Certificate.class, 0);
            }

            @Override
            public void checkClientTrusted(X509Certificate[] certs, String authType) {
                // disable verification
            }

            @Override
            public void checkServerTrusted(X509Certificate[] certs, String authType) {
                // disable verification
            }
        } };

        final SSLContext sc = SSLContext.getInstance("SSL");
        sc.init(null, trustAllCerts, new java.security.SecureRandom());
        HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
        HttpsURLConnection
            .setDefaultHostnameVerifier((String hostname, SSLSession session) -> true);
    }

    @PostConstruct
    public void allowSelfSignedSslCertificates() throws GeneralSecurityException {
        log.info("Disabling SSL validation to allow self signed SSL certificates");
        disableSslVerification();
        final SSLContext sc = SSLContext.getInstance("SSL");
        sc.init(null, new TrustManager[]{new NonValidatingFactory.NonValidatingTM()},
            new SecureRandom());
        HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
        HttpsURLConnection.setDefaultHostnameVerifier((hostname, session) -> true);
    }

}
