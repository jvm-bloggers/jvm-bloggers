package com.jvm_bloggers.frontend.public_area.blogs.timeline_panel

import com.google.common.collect.Lists
import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import com.jvm_bloggers.frontend.admin_area.blogs.BlogPostModel
import com.jvm_bloggers.frontend.admin_area.blogs.BlogPostsPageRequestHandler
import com.jvm_bloggers.utils.NowProvider
import org.apache.wicket.util.tester.TagTester

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.Month

import static com.jvm_bloggers.frontend.public_area.blogs.timeline_panel.BlogPostsTimelinePanel.BLOG_POST_DETAILS_ID
import static com.jvm_bloggers.frontend.public_area.blogs.timeline_panel.BlogPostsTimelinePanel.LEFT_CSS_CLASS
import static com.jvm_bloggers.frontend.public_area.blogs.timeline_panel.BlogPostsTimelinePanel.RIGHT_OFFSET_5_CSS_CLASS
import static com.jvm_bloggers.frontend.public_area.blogs.timeline_panel.BlogPostsTimelinePanel.YEAR_MONTH_SEPARATOR_ID

class BlogPostsTimelinePanelSpec extends MockSpringContextAwareSpecification {

    BlogPostRepository blogPostRepository = Stub()
    BlogPostsPageRequestHandler requestHandler = Mock()
    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(5)
    NowProvider nowProvider = Mock()

    def "Should render year month separator for each month"() {
        given:
        nowProvider.now() >> LocalDate.of(2017, Month.JANUARY, 1).atStartOfDay()
        List<BlogPost> blogPosts = Lists.asList(
                aBlogPost("url1", "title1", nowProvider.now().minusMonths(1)),
                aBlogPost("url2", "title2", nowProvider.now().minusMonths(2)),
                aBlogPost("url3", "title3", nowProvider.now().minusMonths(3)))
        requestHandler.iterator(_, _) >> blogPosts.iterator()
        requestHandler.size() >> blogPosts.size()
        requestHandler.model(_) >>> [new BlogPostModel(blogPosts.get(0)),
                                     new BlogPostModel(blogPosts.get(1)),
                                     new BlogPostModel(blogPosts.get(2))]

        when:
        tester.startComponentInPage(new BlogPostsTimelinePanel("id", requestHandler))

        then:
        tester.assertContains("grudzień 2016")
        tester.assertContains("listopad 2016")
        tester.assertContains("październik 2016")
    }

    def "Should start rendering post details on the left side of the timeline"() {
        given:
        nowProvider.now() >> LocalDate.of(2017, Month.JANUARY, 1).atStartOfDay()
        List<BlogPost> blogPosts = Lists.asList(aBlogPost("url1", "title1", nowProvider.now().minusMonths(1)))
        requestHandler.iterator(_, _) >> blogPosts.iterator()
        requestHandler.size() >> blogPosts.size()
        requestHandler.model(_) >>> [new BlogPostModel(blogPosts.get(0))]

        when:
        tester.startComponentInPage(new BlogPostsTimelinePanel("id", requestHandler))

        then:
        tester.assertContains(LEFT_CSS_CLASS)
    }

    def "Should render second post details on the right side of the timeline"() {
        given:
        nowProvider.now() >> LocalDate.of(2017, Month.JANUARY, 1).atStartOfDay()
        List<BlogPost> blogPosts = Lists.asList(
                aBlogPost("url1", "title1", nowProvider.now().minusMonths(1)),
                aBlogPost("url2", "title2", nowProvider.now().minusMonths(1)))
        requestHandler.iterator(_, _) >> blogPosts.iterator()
        requestHandler.size() >> blogPosts.size()
        requestHandler.model(_) >>> [new BlogPostModel(blogPosts.get(0)),
                                     new BlogPostModel(blogPosts.get(1))]

        when:
        tester.startComponentInPage(new BlogPostsTimelinePanel("id", requestHandler))

        then:
        List<TagTester> tagTesters = tester.getTagsByWicketId(BLOG_POST_DETAILS_ID)
        tagTesters.size() == 2
        tagTesters[0].getAttributeContains("class", LEFT_CSS_CLASS)
        tagTesters[1].getAttributeContains("class", RIGHT_OFFSET_5_CSS_CLASS)
    }

    def "Should not render month year separator for the same month year date"() {
        given:
        nowProvider.now() >> LocalDate.of(2017, Month.JANUARY, 1).atStartOfDay()
        List<BlogPost> blogPosts = Lists.asList(
                aBlogPost("url1", "title1", nowProvider.now().minusMonths(1)),
                aBlogPost("url2", "title2", nowProvider.now().minusMonths(1)),
                aBlogPost("url3", "title3", nowProvider.now().minusMonths(2)))
        requestHandler.iterator(_, _) >> blogPosts.iterator()
        requestHandler.size() >> blogPosts.size()
        requestHandler.model(_) >>> [new BlogPostModel(blogPosts.get(0)),
                                     new BlogPostModel(blogPosts.get(1)),
                                     new BlogPostModel(blogPosts.get(2))]

        when:
        tester.startComponentInPage(new BlogPostsTimelinePanel("id", requestHandler))

        then:
        tester.getTagsByWicketId(YEAR_MONTH_SEPARATOR_ID).size() == 2
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
    protected void setupContext() {
        addBean(blogPostRepository)
        addBean(nowProvider)
        addBean(paginationConfiguration)
    }
}
