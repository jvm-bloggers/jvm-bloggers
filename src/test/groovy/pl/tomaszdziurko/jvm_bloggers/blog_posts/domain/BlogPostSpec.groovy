package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain

import spock.lang.Specification
import spock.lang.Unroll

import java.time.LocalDateTime
import java.time.Month

class BlogPostSpec extends Specification {

    @Unroll
    def "Should return \"#expectedState\" state when approved is #approved "() {
        given:
            BlogPost blogPost = createBlogPost(approved);
        when:
            String approvalState = blogPost.getApprovalState()
        then:
            approvalState == expectedState
        where:
            approved || expectedState
            true     || "Approved"
            false    || "Rejected"
            null     || " -- "
    }

    private BlogPost createBlogPost(final Boolean approved) {
        return BlogPost.builder().approved(approved).build();
    }

    @Unroll
    def "Should return #expected when isModerated called for post with approved = #approved"() {
        given:
            BlogPost blogPost = createBlogPost(approved)
        when:
            boolean isModerated = blogPost.isModerated()
        then:
            isModerated == expected
        where:
            approved || expected
            true     || true
            false    || true
            null     || false
    }

    @Unroll
    def "Should return whether post is going in newsletter"() {
        given:
            BlogPost blogPost = BlogPost.builder()
                .publishedDate(postPublicationDate)
                .build();
        when:
            boolean inNewsletter = blogPost.isGoingInNewsletter(lastNewsletterDate)
        then:
            inNewsletter == expected
        where:
            postPublicationDate                             || lastNewsletterDate                              || expected
            LocalDateTime.of(2016, Month.MARCH, 20, 12, 00) || LocalDateTime.of(2016, Month.MARCH, 19, 12, 00) || true
            LocalDateTime.of(2016, Month.MARCH, 20, 12, 00) || LocalDateTime.of(2016, Month.MARCH, 21, 12, 00) || false
    }

}
