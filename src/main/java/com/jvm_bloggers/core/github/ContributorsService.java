package com.jvm_bloggers.core.github;

import com.jvm_bloggers.GithubClient;
import com.jvm_bloggers.entities.github.Contributor;

import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.Response;

@Service
public class ContributorsService {

    private static final GenericType<List<Contributor>> CONTRIBUTORS_LIST_TYPE =
        new GenericType<List<Contributor>>() {
        };

    private final Client client;
    private final GithubProperties properties;

    public ContributorsService(@GithubClient Client client, GithubProperties githubProperties) {
        this.client = client;
        this.properties = githubProperties;
    }

    @Cacheable("contributors")
    public List<Contributor> fetchContributors() {
        WebTarget target = client
            .target("{api_url}/repos/{org}/{repo}/contributors")
            .resolveTemplate("api_url", properties.getApiUrl(), false)
            .resolveTemplate("org", properties.getOrg(), false)
            .resolveTemplate("repo", properties.getRepo(), false);
        return traversePages(target).collect(Collectors.toList());
    }

    private Stream<Contributor> traversePages(WebTarget target) {
        Response response = target.request().get();

        Stream<Contributor> currentPage = response.readEntity(CONTRIBUTORS_LIST_TYPE).stream();

        return Optional.ofNullable(response.getLink("next"))
            .map(client::target)
            .map(this::traversePages)
            .map(nextPage -> Stream.concat(currentPage, nextPage))
            .orElse(currentPage);
    }
}
