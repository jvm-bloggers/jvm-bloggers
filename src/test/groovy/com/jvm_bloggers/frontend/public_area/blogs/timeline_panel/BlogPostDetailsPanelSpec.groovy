package com.jvm_bloggers.frontend.public_area.blogs.timeline_panel

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import org.apache.wicket.util.tester.TagTester

import java.time.LocalDateTime

import static com.jvm_bloggers.frontend.public_area.blogs.timeline_panel.BlogPostDetailsPanel.LINK_ID
import static com.jvm_bloggers.frontend.public_area.blogs.timeline_panel.BlogPostDetailsPanel.PUBLISHED_DATE_ID
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER

class BlogPostDetailsPanelSpec extends MockSpringContextAwareSpecification {

    def "Should render panel"() {
        given:
        BlogPost blogPost = aBlogPost("url", "title", LocalDateTime.now())

        when:
        tester.startComponentInPage(new BlogPostDetailsPanel("id", "thisClass", blogPost))

        then:
        TagTester panelTagTester = tester.getTagByWicketId("id")
        panelTagTester.getAttributeContains("class", "thisClass")
        TagTester linkTagTester = tester.getTagByWicketId(LINK_ID)
        linkTagTester.getAttributeContains("href", blogPost.url)
        linkTagTester.value == blogPost.title
        tester.assertLabel("id:$PUBLISHED_DATE_ID", blogPost.publishedDate.format(DATE_TIME_FORMATTER))
    }

    private BlogPost aBlogPost(String url, String title, LocalDateTime publishedDate) {
        new BlogPost.BlogPostBuilder()
                .url(url)
                .blog(new Blog())
                .publishedDate(publishedDate)
                .title(title)
                .build()
    }

    @Override
    protected void setupContext() {}
}
