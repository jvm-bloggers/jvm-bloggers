package com.jvm_bloggers.core.github;

import org.apache.wicket.util.string.Strings;
import org.springframework.stereotype.Component;

import java.io.IOException;

import javax.ws.rs.client.ClientRequestContext;
import javax.ws.rs.client.ClientRequestFilter;
import javax.ws.rs.core.HttpHeaders;

@Component
public class GithubAuthenticationFilter implements ClientRequestFilter {

    private final GithubProperties githubProperties;

    public GithubAuthenticationFilter(GithubProperties githubProperties) {
        this.githubProperties = githubProperties;
    }

    @Override
    public void filter(ClientRequestContext requestContext) throws IOException {
        if (!Strings.isEmpty(githubProperties.getToken())) {
            requestContext
                .getHeaders()
                .putSingle(HttpHeaders.AUTHORIZATION, "token " + githubProperties.getToken());
        }
    }
}
