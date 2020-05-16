package com.jvm_bloggers.entities.blog_post

import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.ZoneTimeProvider
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll
import java.time.LocalDateTime

import static com.jvm_bloggers.ObjectMother.aBlogPost
import static java.time.Month.*

@Subject(BlogPost)
class BlogPostSpec extends Specification {
    private static final Boolean NOT_MODERATED = null

    private NowProvider nowProvider = new ZoneTimeProvider()

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
    def "Should return #expected whether post with approval #approved posted on #postApprovedDate is going in newsletter if last issue date was #lastNewsletterDate"() {

        given:
        BlogPost blogPost = aBlogPost(approved: approved, approvedDate: postApprovedDate)

        when:
        boolean inNewsletter = blogPost.isGoingInNewsletter(lastNewsletterDate)

        then:
        inNewsletter == expected

        where:
        approved | postApprovedDate                             || lastNewsletterDate               || expected
        true     | LocalDateTime.of(2016, MARCH, 20, 12, 00)    || postApprovedDate.minusDays(1)    || true
        true     | LocalDateTime.of(1999, DECEMBER, 31, 23, 59) || postApprovedDate.plusDays(1)     || false
        true     | LocalDateTime.of(2016, MARCH, 20, 12, 00)    || postApprovedDate.minusMinutes(1) || true
        true     | LocalDateTime.of(2020, JANUARY, 1, 00, 00)   || postApprovedDate.plusMinutes(1)  || false
        false    | LocalDateTime.of(2016, MARCH, 20, 12, 00)    || postApprovedDate.minusDays(1)    || false
        false    | null                                         || LocalDateTime.now()              || false
        true     | LocalDateTime.now()                          || postApprovedDate.plusHours(1)    || false
        true     | LocalDateTime.now().plusDays(1)              || postApprovedDate.minusHours(1)   || true
    }

    def "Should approve post"() {
        given:
        BlogPost blogPost = aBlogPost(approved: NOT_MODERATED)
        LocalDateTime approvedDate = nowProvider.now()

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
