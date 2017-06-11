package com.jvm_bloggers.domain.query

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.click.Click
import com.jvm_bloggers.entities.click.ClickRepository
import com.jvm_bloggers.entities.click.PostIdWithCount
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject
import javaslang.collection.List as JavaslangList

import java.time.LocalDateTime

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class MostPopularBlogPostsQueryIntegrationSpec extends SpringContextAwareSpecification {

    static LocalDateTime START = LocalDateTime.of(2017, 07, 22, 10, 10)
    static LocalDateTime END  = START.plusMonths(1)

    @Subject
    @Autowired
    MostPopularBlogPostsQuery mostPopularBlogPostsQuery

    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    @Autowired
    ClickRepository clickRepository

    def "should calculate top blog posts from given period"() {
        given: "there are ten personal blogs"
        List<Blog> personalBlogs = (1..10).collect { it ->
            saveBlog("Blogger $it", "exampleRss $it", PERSONAL)
        }

        and: "each blog has a one post"
        List<BlogPost> personalPosts = personalBlogs.collect { b ->
            savePost(b.id, START, true, b)
        }

        and: "posts have increasing number of clicks"
        Collections.shuffle(personalBlogs)
        personalPosts.withIndex(1).forEach({ it ->
            int index = it.getSecond()
            BlogPost post = it.getFirst()
            for (int i = 0; i < index; i++) {
                clickRepository.save(new Click(post, START))
            }
        })

        and: "there are some clicks outside of analysed period"
        saveClicksForPost(personalPosts[0], END.plusMinutes(1), 50)
        saveClicksForPost(personalPosts[1], START.minusMinutes(1), 50)

        when:
        JavaslangList<PostIdWithCount> popularPosts = mostPopularBlogPostsQuery.getBestPersonalPosts(START, END, 5)

        then: "last post from collection should have most clicks"
        popularPosts.size() == 5
        popularPosts.first().blogPostId == personalPosts.last().id
        popularPosts.first().count == 10

        and: "each clicks count should be one less starting from 10"
        popularPosts.collect {it.count} == [10, 9, 8, 7, 6]
    }

    private Blog saveBlog(String author, String rssUrl, BlogType type) {
        return blogRepository.save(
            Blog.builder()
                .jsonId(1L)
                .author(author)
                .rss(rssUrl)
                .url("url")
                .dateAdded(START)
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
                .title("title" + index)
                .url("url" + index)
                .build()
            )
    }

    private void saveClicksForPost(BlogPost post, LocalDateTime dateTime, int howMany) {
        for (int i = 0; i < howMany; i++) {
            clickRepository.save(new Click(post, dateTime))
        }
    }

}
