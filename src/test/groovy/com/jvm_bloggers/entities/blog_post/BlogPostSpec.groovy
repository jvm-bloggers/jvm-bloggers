package com.jvm_bloggers.entities.blog_post

import com.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime
import java.time.Month

import static com.jvm_bloggers.ObjectMother.aBlogPost
import static java.time.LocalDateTime.of

@Subject(BlogPost)
class BlogPostSpec extends Specification {
    private static final Boolean NOT_MODERATED = null

    @Unroll
    def "Should return \"#expectedState\" state when approved is #approved "() {
        given:
        BlogPost blogPost = aBlogPost(approved: approved)

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
        BlogPost blogPost = aBlogPost(approved: approved)

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
        BlogPost blogPost = aBlogPost(approved: approved, approvedDate: postApprovedDate)

        when:
        boolean inNewsletter = blogPost.isGoingInNewsletter(lastNewsletterDate)

        then:
        inNewsletter == expected

        where:
        approved | postApprovedDate                  || lastNewsletterDate                || expected
        true     | of(2016, Month.MARCH, 20, 12, 00) || of(2016, Month.MARCH, 19, 12, 00) || true
        true     | of(2016, Month.MARCH, 20, 12, 00) || of(2016, Month.MARCH, 21, 12, 00) || false
        false    | of(2016, Month.MARCH, 20, 12, 00) || of(2016, Month.MARCH, 19, 12, 00) || false
    }

    def "Should approve post"() {
        given:
        BlogPost blogPost = aBlogPost(approved: NOT_MODERATED)
        LocalDateTime approvedDate = new NowProvider().now()

        when:
        blogPost.approve(approvedDate)

        then:
        blogPost.approved == Boolean.TRUE
        blogPost.approvedDate == approvedDate
    }

    def "Should create BlogPost with random uid"() {
        when:
        BlogPost blogPost = aBlogPost(approved: false)

        then:
        blogPost.uid != null
    }
}
