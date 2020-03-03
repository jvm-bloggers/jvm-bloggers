package com.jvm_bloggers.entities.blog_post

import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.ZoneTimeProvider
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime
import java.time.Month

@Subject(BlogPost)
class BlogPostSpec extends Specification {
    private static final Boolean NOT_MODERATED = null

    private NowProvider nowProvider = new ZoneTimeProvider()

    @Unroll
    def "Should return \"#expectedState\" state when approved is #approved "() {
        given:
        BlogPost blogPost = createBlogPost(approved)

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
        BlogPost blogPost = createBlogPost(approved, postApprovedDate)

        when:
        boolean inNewsletter = blogPost.isGoingInNewsletter(lastNewsletterDate)

        then:
        inNewsletter == expected

        where:
        approved | postApprovedDate                                || lastNewsletterDate                              || expected
        true     | LocalDateTime.of(2016, Month.MARCH, 20, 12, 00) || LocalDateTime.of(2016, Month.MARCH, 19, 12, 00) || true
        true     | LocalDateTime.of(2016, Month.MARCH, 20, 12, 00) || LocalDateTime.of(2016, Month.MARCH, 21, 12, 00) || false
        false    | LocalDateTime.of(2016, Month.MARCH, 20, 12, 00) || LocalDateTime.of(2016, Month.MARCH, 19, 12, 00) || false
    }

    def "Should approve post"() {
        given:
        BlogPost blogPost = createBlogPost(NOT_MODERATED)
        LocalDateTime approvedDate = nowProvider.now()

        when:
        blogPost.approve(approvedDate)

        then:
        blogPost.approved == Boolean.TRUE
        blogPost.approvedDate == approvedDate
    }

    private BlogPost createBlogPost(final Boolean approved) {
        createBlogPost(approved,
            Boolean.TRUE == approved
                ? nowProvider.now()
                : null)
    }

    private BlogPost createBlogPost(final Boolean approved, LocalDateTime postApprovedDate) {

        return BlogPost.builder()
            .approved(approved)
            .title("title")
            .url("url")
            .publishedDate(nowProvider.now())
            .approvedDate(postApprovedDate)
            .blog(Blog.builder()
            .bookmarkableId("bookmarkableId")
            .blogType(BlogType.PERSONAL)
            .author("author")
            .rss("rss")
            .url("url")
            .dateAdded(nowProvider.now())
            .build())
            .build()
    }

    def "Should create BlogPost with random uid"() {
        when:
        BlogPost blogPost = createBlogPost(false)

        then:
        blogPost.uid != null
    }
}
