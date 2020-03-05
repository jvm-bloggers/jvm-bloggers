package com.jvm_bloggers.core.social.twitter

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.ZoneTimeProvider
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

@Subject(TweetContentGenerator)
class TweetContentGeneratorSpec extends Specification {

    private static final long ISSUE_NUMBER = 999L
    private static final String LINK = "http://jvm-bloggers.com/issue/$ISSUE_NUMBER"

    private static final Random randomJsonId = new Random()
    private static final NowProvider nowProvider = new ZoneTimeProvider()

    private LinkGenerator linkGenerator = Stub(LinkGenerator)

    private TweetContentGenerator contentGenerator = new TweetContentGenerator(this.linkGenerator)

    def setup() {
        linkGenerator.generateIssueLink(_) >> { args -> LINK }
    }

    def "Should generate a Tweet content with an issue number and link"() {
        given:
        NewsletterIssue issue = NewsletterIssue
                .builder()
                .issueNumber(ISSUE_NUMBER)
                .heading("issue heading")
                .blogPosts(posts())
                .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        tweetContent.contains(issue.issueNumber as String)
        tweetContent.contains(LINK)
    }

    def "Should add two twitter handles of personal blogs"() {
        given:
        NewsletterIssue issue = NewsletterIssue
                .builder()
                .issueNumber(ISSUE_NUMBER)
                .heading("issue heading")
                .blogPosts(posts())
                .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        def personal = /@personal/
        def personalBlogs = (tweetContent =~ /$personal/)
        assert personalBlogs.count == 2
    }

    def "Should add one twitter handle of company blog"() {
        given:
        NewsletterIssue issue = NewsletterIssue
                .builder()
                .issueNumber(ISSUE_NUMBER)
                .heading("issue heading")
                .blogPosts(posts())
                .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        def company = /@company/
        def companyBlogs = (tweetContent =~ /$company/)
        assert companyBlogs.count == 1
    }

    def "Should add company twitter handle as the second on handles list"() {
        given:
        NewsletterIssue issue = NewsletterIssue
                .builder()
                .issueNumber(ISSUE_NUMBER)
                .heading("issue heading")
                .blogPosts(posts())
                .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        def handles = /.*@personal\d{1}, @company\d{1} i @personal\d{1}.*/
        tweetContent ==~ /$handles/
    }

    def "Should add no company handle if there are no company blogs"() {
        given:
        NewsletterIssue issue = NewsletterIssue
                .builder()
                .issueNumber(ISSUE_NUMBER)
                .heading("issue heading")
                .blogPosts(noCompanyPosts())
                .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        def company = /@company/
        def companyBlogs = (tweetContent =~ /$company/)
        assert companyBlogs.count == 0
    }

    def "Should add one personal and one company handles only if there is no second personal handle"() {
        given:
        NewsletterIssue issue = NewsletterIssue
                .builder()
                .issueNumber(ISSUE_NUMBER)
                .heading("issue heading")
                .blogPosts(singlePersonalPost())
                .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        def personal = /@personal/
        def personalBlogs = (tweetContent =~ /$personal/)
        assert personalBlogs.count == 1

        and:
        def company = /@company/
        def companyBlogs = (tweetContent =~ /$company/)
        assert companyBlogs.count == 1
    }

    @Unroll
    def "Should always have java and jvm tags at the end"() {
        given:
        NewsletterIssue issue = NewsletterIssue
                .builder()
                .issueNumber(ISSUE_NUMBER)
                .heading("issue heading")
                .blogPosts(posts)
                .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        tweetContent.endsWith("#java #jvm")

        where:
        posts << [posts(), postsWithLongHandles()]
    }

    def "should have just two handles if twitter is missing for a third one"() {
        given:
        NewsletterIssue issue = NewsletterIssue
            .builder()
            .issueNumber(ISSUE_NUMBER)
            .heading("issue heading")
            .blogPosts(notAllHavingTwitterHandlePosts())
            .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        def handles = /.*między innymi @personal1 i @company1.*/
        tweetContent ==~ /$handles/
    }

    def "Should add two distinct twitter handles of personal blogs"() {
        given:
        NewsletterIssue issue = NewsletterIssue
            .builder()
            .issueNumber(ISSUE_NUMBER)
            .heading("issue heading")
            .blogPosts(twoDistinctPersonalTwitterHandlesWithOneDuplication())
            .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        def personal1 = /@personal1/
        def personal1Blog = (tweetContent =~ /$personal1/)
        assert personal1Blog.count == 1

        and:
        def personal2 = /@personal2/
        def personal2Blog = (tweetContent =~ /$personal2/)
        assert personal2Blog.count == 1
    }

    def "Should add only one personal handle if there is no other distinct"() {
        given:
        NewsletterIssue issue = NewsletterIssue
            .builder()
            .issueNumber(ISSUE_NUMBER)
            .heading("issue heading")
            .blogPosts(onlyOneDistinctPersonalTwitterHandle())
            .build()

        when:
        String tweetContent = contentGenerator.generateTweetContent(issue)

        then:
        def personal = /@personal/
        def personalBlogs = (tweetContent =~ /$personal/)
        assert personalBlogs.count == 1
    }

    private Collection<BlogPost> notAllHavingTwitterHandlePosts() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("@company1", COMPANY)))
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        posts.add(blogPost(blog(PERSONAL)))
        return posts
    }

    private Collection<BlogPost> noCompanyPosts() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        posts.add(blogPost(blog("@personal2", PERSONAL)))
        posts.add(blogPost(blog("@personal3", PERSONAL)))
        return posts
    }

    private Collection<BlogPost> singlePersonalPost() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        posts.add(blogPost(blog("@company1", COMPANY)))
        posts.add(blogPost(blog("@company2", COMPANY)))
        return posts
    }

    private Collection<BlogPost> posts() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        posts.add(blogPost(blog("@personal2", PERSONAL)))
        posts.add(blogPost(blog("@company1", COMPANY)))
        posts.add(blogPost(blog("@company2", COMPANY)))
        posts.add(blogPost(blog("@personal3", PERSONAL)))
        return posts
    }

    private Collection<BlogPost> postsWithLongHandles() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("@veryLongPersonalHandle1", PERSONAL)))
        posts.add(blogPost(blog("@veryLongPersonalHandle2", PERSONAL)))
        posts.add(blogPost(blog("@veryLongCompanyHandle1", COMPANY)))
        posts.add(blogPost(blog("@veryLongCompanyHandle2", COMPANY)))
        posts.add(blogPost(blog("@veryLongPersonalHandle3", PERSONAL)))
        return posts
    }

    private Collection<BlogPost> twoDistinctPersonalTwitterHandlesWithOneDuplication() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        posts.add(blogPost(blog("@personal2", PERSONAL)))
        return posts
    }

    private Collection<BlogPost> onlyOneDistinctPersonalTwitterHandle() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        return posts
    }

    private BlogPost blogPost(Blog blog) {
        BlogPost
            .builder()
            .title("title")
            .url("url")
            .publishedDate(nowProvider.now())
            .blog(blog)
            .build()
    }

    private Blog blog(String twitterHandle, BlogType blogType) {
        Blog.builder()
            .bookmarkableId(randomJsonId.nextLong().toString())
            .author("author")
            .twitter(twitterHandle)
            .rss("rss")
            .url("url")
            .dateAdded(nowProvider.now())
            .blogType(blogType)
            .build()
    }

    private Blog blog(BlogType blogType) {
        blog(null, blogType)
    }

}
