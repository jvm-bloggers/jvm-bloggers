package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain

import spock.lang.Specification
import spock.lang.Unroll

class BlogPostSpec extends Specification {

    @Unroll
    def "Should return \"#expectedState\" state when approved is #approved "() {
        given:
            BlogPost blogPost = new BlogPost(approved: approved)
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

    @Unroll
    def "Should return #expected when isModerated called for post with approved = #approved"() {
        given:
            BlogPost blogPost = new BlogPost(approved: approved)
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
    def "Should return #expected when isNotModerated called for post with approved = #approved"() {
        given:
            BlogPost  blogPost = new BlogPost(approved: approved)
        when:
            boolean isNotModerated = blogPost.isNotModerated()
        then:
            isNotModerated == expected
        where:
            approved || expected
            true     || false
            false    || false
            null     || true
    }
}
