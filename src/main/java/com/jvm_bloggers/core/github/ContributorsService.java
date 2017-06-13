package com.jvm_bloggers.core.github;

import com.jvm_bloggers.GithubClient;
import com.jvm_bloggers.entities.github.Contributor;
import io.vavr.collection.List;
import io.vavr.collection.Seq;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.Response;

@Service
public class ContributorsService {

    private static final GenericType<java.util.List<Contributor>> CONTRIBUTORS_LIST_TYPE =
        new GenericType<java.util.List<Contributor>>() {
        };

    private final Client client;
    private final GithubProperties properties;

    public ContributorsService(@GithubClient Client client, GithubProperties githubProperties) {
        this.client = client;
        this.properties = githubProperties;
    }

    @Cacheable("contributors")
    public Seq<Contributor> fetchContributors() {
        WebTarget target = client
            .target("{api_url}/repos/{org}/{repo}/contributors?per_page={page_size}")
            .resolveTemplate("api_url", properties.getApiUrl(), false)
            .resolveTemplate("org", properties.getOrg(), false)
            .resolveTemplate("repo", properties.getRepo(), false)
            .resolveTemplate("page_size", properties.getPageSize(), false);
        return List.ofAll(getContributors(target));
    }

    private java.util.List<Contributor> getContributors(WebTarget target) {
        Response response = target.request().get();
        return response.readEntity(CONTRIBUTORS_LIST_TYPE);
    }
}
