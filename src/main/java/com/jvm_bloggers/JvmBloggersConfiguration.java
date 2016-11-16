package com.jvm_bloggers;


import akka.actor.ActorSystem;
import akka.stream.ActorMaterializer;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.CacheManager;
import org.springframework.cache.guava.GuavaCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;

@Configuration
public class JvmBloggersConfiguration {

    @Bean
    public ActorSystem getActorSystem() {
        return ActorSystem.create("jvm-bloggers-akka");
    }

    @Bean
    public ActorMaterializer getActorMaterializer(ActorSystem actorSystem) {
        return ActorMaterializer.create(actorSystem);
    }

    @Bean
    public Client getMailingRestClient(@Value("${mailing.apiKey}") String malingApiKey) {
        final Client client = ClientBuilder.newClient();

        HttpAuthenticationFeature authFeature;
        authFeature = HttpAuthenticationFeature
            .basicBuilder()
            .nonPreemptive()
            .credentials("api", malingApiKey)
            .build();
        client.register(authFeature);
        return client;
    }

    @Bean
    public CacheManager cacheManager() {
        return new GuavaCacheManager();
    }

    @Bean
    public ObjectMapper jsonObjectMapper() {
        return new ObjectMapper();
    }
    
    @Bean
    public ExecutorService singleThreadExecutor() {
        return Executors.newSingleThreadExecutor();
    }
}
