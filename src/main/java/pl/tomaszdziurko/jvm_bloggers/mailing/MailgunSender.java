package pl.tomaszdziurko.jvm_bloggers.mailing;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Component
@Slf4j
public class MailgunSender implements MailSender {

    private String senderAddress;
    private Client mailingRestClient;

    public MailgunSender() {

    }

    @Autowired
    public MailgunSender(Client mailingRestClient, @Value("${mailing.fromEmail}") String senderAddress) {
        this.mailingRestClient = mailingRestClient;
        this.senderAddress = senderAddress;
    }

    @Override
    public void sendEmail(String recipientAddress, String subject, String htmlContent) {
        Form form = prepareEmail(recipientAddress, subject, htmlContent);
        WebTarget webTarget = mailingRestClient.
            target("https://api.mailgun.net/v3/jvm-bloggers.com").
            path("messages");
        log.info("Sending mail " + form.asMap().values().toString());

        Invocation.Builder invocationBuilder;
        invocationBuilder = webTarget.request(MediaType.APPLICATION_JSON_TYPE);
        Response response = invocationBuilder.post(Entity.form(form));
        int status = response.getStatus();
        log.info("Sending status = " + status);

        if (response.hasEntity()) {
            String json = response.readEntity(String.class);
            log.info("Response = " + json);
        }
    }

    private Form prepareEmail(String recipientAddress, String subject, String htmlContent) {
        Form form = new Form();
        form.param("to", recipientAddress);
        form.param("from", FROM_NAME + " <" + senderAddress + ">");
        form.param("subject", subject);
        form.param("html", htmlContent);
        form.param("h:Reply-To", senderAddress);
        return form;
    }

}
