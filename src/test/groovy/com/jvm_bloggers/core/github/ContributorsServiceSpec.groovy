package com.jvm_bloggers.core.github

import com.jvm_bloggers.entities.github.Contributor
import io.vavr.collection.List as VavrList
import spock.lang.Specification
import spock.lang.Subject

import javax.ws.rs.client.Client
import javax.ws.rs.client.Invocation
import javax.ws.rs.client.WebTarget
import javax.ws.rs.core.Response

import static org.assertj.core.api.Assertions.assertThat

@Subject(ContributorsService)
class ContributorsServiceSpec extends Specification {

    public static final String ORG = 'ORG'
    public static final String REPO = 'REPO'
    public static final String URL = 'URL'

    ContributorsService testObj

    Client client = Mock()
    GithubProperties properties = new GithubProperties()

    def setup() {
        properties.setOrg(ORG)
        properties.setRepo(REPO)
        properties.setApiUrl(URL)
        testObj = new ContributorsService(client, properties)
    }

    def "Should fetch contributors from a single page"() {
        given:
        WebTarget target = Mock()
        client.target(_) >> target
        target.resolveTemplate(_, _, _) >> target

        Response response = Mock(Response)
        Contributor contributor1 = Stub(Contributor)
        Contributor contributor2 = Stub(Contributor)
        response.readEntity(_) >> Arrays.asList(contributor1, contributor2)

        target.request() >> Stub(Invocation.Builder) {
            get() >> response
        }

        when:
        VavrList<Contributor> actual = testObj.fetchContributors()

        then:
        assertThat(actual).containsOnly(contributor1, contributor2)
    }
}
