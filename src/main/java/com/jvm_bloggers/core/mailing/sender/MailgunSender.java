package com.jvm_bloggers.core.mailing.sender;

import com.google.common.util.concurrent.RateLimiter;
import com.jvm_bloggers.MailingClient;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import static com.jvm_bloggers.ApplicationProfiles.PRODUCTION;

@Component
@Profile({PRODUCTION})
@Slf4j
class MailgunSender implements MailSender {

    private Client mailingRestClient;
    private RateLimiter rateLimiter;

    public MailgunSender() {

    }

    @Autowired
    public MailgunSender(@MailingClient Client mailingRestClient,
                         @Value("${mailing.throttleDelayInSeconds}") long throttleTimeInSeconds) {
        this.mailingRestClient = mailingRestClient;
        this.rateLimiter = RateLimiter.create(1.0 / throttleTimeInSeconds);
    }

    @Override
    public EmailSendingStatus sendEmail(String fromAddress, String toAddress,
                                        String subject, String htmlContent) {

        double timeToAcquire = rateLimiter.acquire();
        log.info("Acquired rate limiter for mail sending after {} seconds", timeToAcquire);
        Form form = prepareEmail(fromAddress, toAddress, subject, htmlContent);
        WebTarget webTarget = mailingRestClient
            .target("https://api.mailgun.net/v3/jvm-bloggers.com")
            .path("messages");
        log.info("Sending mail '{}' to {}", subject, toAddress);

        Invocation.Builder invocationBuilder;
        invocationBuilder = webTarget.request(MediaType.APPLICATION_JSON_TYPE);
        Response response = invocationBuilder.post(Entity.form(form));
        int status = response.getStatus();
        log.info("Sending status = " + status);

        if (response.hasEntity()) {
            String json = response.readEntity(String.class);
            log.info("Response = " + json);
        }

        return status == HttpStatus.OK.value() ? EmailSendingStatus.SUCCESS
            : EmailSendingStatus.ERROR;
    }

    private Form prepareEmail(String fromAddress, String toAddress, String subject,
                              String htmlContent) {
        Form form = new Form();
        form.param("from", fromAddress);
        form.param("to", toAddress);
        form.param("subject", subject);
        form.param("html", htmlContent);
        form.param("h:Reply-To", fromAddress);
        return form;
    }

}
