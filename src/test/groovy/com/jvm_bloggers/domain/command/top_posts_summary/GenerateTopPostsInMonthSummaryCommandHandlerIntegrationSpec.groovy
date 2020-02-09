package com.jvm_bloggers.domain.command.top_posts_summary

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.click.Click
import com.jvm_bloggers.entities.click.ClickRepository
import com.jvm_bloggers.entities.fb.FacebookPostRepository
import com.jvm_bloggers.entities.top_posts_summary.PopularPersonalPost
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummary
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummaryRepository
import io.vavr.control.Option
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

import java.time.LocalDateTime
import java.time.YearMonth

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

@Subject(GenerateTopPostsInMonthSummaryCommandHandler)
class GenerateTopPostsInMonthSummaryCommandHandlerIntegrationSpec extends SpringContextAwareSpecification {

    static YearMonth ANALYZED_MONTH = YearMonth.of(2017, 7)
    static LocalDateTime START_OF_MONTH = ANALYZED_MONTH.atDay(1).atStartOfDay()

    @Autowired
    GenerateTopPostsInMonthSummaryCommandHandler generateTopPostsCommandHandler

    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    @Autowired
    ClickRepository clickRepository

    @Autowired
    TopPostsSummaryRepository topPostsSummaryRepository

    @Autowired
    FacebookPostRepository facebookPostRepository

    def 'should calculate and store top blog posts from given period'() {
        given: 'there are ten personal blogs'
        List<Blog> personalBlogs = (1..10).collect { it ->
            saveBlog(it, "Blogger $it", "exampleRss $it", PERSONAL)
        }

        and: 'each blog has a one post'
        List<BlogPost> personalPosts = personalBlogs.collect { b ->
            savePost(b.id, START_OF_MONTH, true, b)
        }

        and: 'posts have increasing number of clicks'
        Collections.shuffle(personalBlogs)
        personalPosts.withIndex(1).forEach({ it ->
            int index = it.getSecond()
            BlogPost post = it.getFirst()
            for (int i = 0; i < index; i++) {
                clickRepository.save(new Click(
                    post, START_OF_MONTH, 'anyIp', 'anyReferer', 'anyUserAgent')
                )
            }
        })

        and: 'there are some clicks outside of analysed period'
        saveClicksForPost(personalPosts[0], START_OF_MONTH.plusMonths(2), 50)
        saveClicksForPost(personalPosts[1], START_OF_MONTH.minusMinutes(1), 50)

        when:
        generateTopPostsCommandHandler.handle(
            new GenerateTopPostsInMonthSummary(ANALYZED_MONTH, 5, 5)
        )

        then: 'last post from collection should have most clicks'
        Option<TopPostsSummary> postsSummary = topPostsSummaryRepository
            .findOneByYearAndMonth(ANALYZED_MONTH.getYear(), ANALYZED_MONTH.getMonthValue())

        postsSummary.isDefined()
        List<PopularPersonalPost> popularPosts = postsSummary.get().getPopularPersonalPosts()
        popularPosts.size() == 5
        popularPosts.first().getBlogPost() == personalPosts.last()
        popularPosts.first().getPosition() == 1
        popularPosts.first().getNumberOfClicks() == 10

        and: 'each clicks count should be one less starting from 10'
        popularPosts.collect {it.getNumberOfClicks()} == [10L, 9L, 8L, 7L, 6L]

        and: 'there was an event generated informing about successful summary generation'
        !facebookPostRepository.findAll().isEmpty()
    }

    private Blog saveBlog(Long nr,String author, String rssUrl, BlogType type) {
        return blogRepository.save(
            Blog.builder()
                .bookmarkableId('bookmarkableId' + nr)
                .author(author)
                .rss(rssUrl)
                .url('url')
                .moderationRequired(false)
                .dateAdded(START_OF_MONTH)
                .blogType(type)
                .build())
    }

    private BlogPost savePost(final long index, final LocalDateTime publishedDate,
                              final Boolean approved, final Blog blog) {
        return blogPostRepository
            .save(
                BlogPost.builder()
                .publishedDate(publishedDate)
                .approved(approved)
                .blog(blog)
                .title('title' + index)
                .url('url' + index)
                .build()
            )
    }

    private void saveClicksForPost(BlogPost post, LocalDateTime dateTime, int howMany) {
        for (int i = 0; i < howMany; i++) {
            clickRepository.save(
                new Click(post, dateTime, 'anyIp', 'anyReferer', 'anyUserAgent')
            )
        }
    }

}
