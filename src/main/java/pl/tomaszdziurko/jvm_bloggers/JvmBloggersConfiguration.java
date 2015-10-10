package pl.tomaszdziurko.jvm_bloggers;


import akka.actor.ActorSystem;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;

@Configuration
public class JvmBloggersConfiguration {

    @Bean
    public ActorSystem getActorSystem() {
        ActorSystem system = ActorSystem.create("jvm-bloggers-akka");
        return system;
    }

    @Bean
    public Client getMailingRestClient(@Value("${mailing.apiKey}") String malingApiKey) {
        final Client client = ClientBuilder.newClient();

        HttpAuthenticationFeature authFeature;
        authFeature = HttpAuthenticationFeature.
            basicBuilder().
            nonPreemptive().
            credentials("api", malingApiKey).
            build();
        client.register(authFeature);
        return client;
    }

}
