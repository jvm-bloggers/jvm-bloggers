package com.jvm_bloggers;

import akka.actor.ActorSystem;

import com.jvm_bloggers.core.github.GithubAuthenticationFilter;
import com.jvm_bloggers.core.github.GithubProperties;
import com.jvm_bloggers.core.social.fb.publisher.FacebookConfiguration;
import com.jvm_bloggers.core.social.twitter.publisher.TwitterConfiguration;

import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;

@Configuration
@EnableConfigurationProperties(
    {GithubProperties.class, FacebookConfiguration.class, TwitterConfiguration.class})
public class JvmBloggersConfiguration {

    @Bean
    public ActorSystem getActorSystem() {
        return ActorSystem.create("jvm-bloggers-akka");
    }

    @Bean
    @MailingClient
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
    @GithubClient
    public Client getGithubRestClient(GithubAuthenticationFilter githubAuthenticationFilter) {
        final Client client = ClientBuilder.newClient();
        client.register(githubAuthenticationFilter);

        return client;
    }

    @Bean
    public CacheManager cacheManager(@Value("${cache.spec}") String cacheSpec) {
        CaffeineCacheManager cacheManager = new CaffeineCacheManager();
        cacheManager.setCacheSpecification(cacheSpec);
        return cacheManager;
    }

    @Bean
    public ExecutorService singleThreadExecutor() {
        return Executors.newSingleThreadExecutor();
    }
}
