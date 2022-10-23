package com.jvm_bloggers.entities.blog_post

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.tag.Tag
import org.apache.commons.lang3.StringUtils
import org.hibernate.search.mapper.orm.Search
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

import javax.persistence.EntityManager
import javax.persistence.EntityManagerFactory
import java.time.LocalDateTime

import static com.jvm_bloggers.ObjectMother.aBlog
import static com.jvm_bloggers.ObjectMother.aBlogPost
import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

@Subject(BlogPostTextSearchRepository)
class BlogPostTextSearchRepositorySpec extends SpringContextAwareSpecification {

    @Autowired
    private EntityManagerFactory entityManagerFactory
    private EntityManager entityManager
    private BlogPostTextSearchRepository blogPostTextSearchRepository

    def setup() {
        entityManager = entityManagerFactory.createEntityManager()
        entityManager.getTransaction().begin()
        blogPostTextSearchRepository = new BlogPostTextSearchRepositoryImpl(entityManager)
    }

    def cleanup() {
        entityManager.getTransaction().rollback()
        entityManager.close()
    }

    def "should find approved blog posts containing given keyword in title"() {
        given:
        Blog personalBlog = aBlog(blogType: PERSONAL)
        Blog companyBlog = aBlog(author: 'title company', blogType: COMPANY)
        Tag functionalProgrammingTag = new Tag('Functional Programming')
        BlogPost javaHttpClient = aBlogPost(title: 'Java 9 HTTP Client', approved: false, blog: personalBlog)
        BlogPost javaConcurrencyBlogPost = aBlogPost(title: 'Java Concurrency', approved: true, blog: personalBlog)
        BlogPost lambdaExpressionsInJavaBlogPost = aBlogPost(title: 'Lambda Expressions in JAVA', approved: true, blog: companyBlog, tags: [functionalProgrammingTag])
        BlogPost parallelStreamsBlogPost = aBlogPost(title: 'Parallel Streams', approved: true, blog: personalBlog, tags: [functionalProgrammingTag])

        flushData([personalBlog, companyBlog],
                [functionalProgrammingTag],
                [javaHttpClient, javaConcurrencyBlogPost, lambdaExpressionsInJavaBlogPost, parallelStreamsBlogPost])

        String keyword = 'Java'
        int acceptedPostsWithGivenKeywordInTitleCount = 2
        when:
        List<BlogPost> searchResult = blogPostTextSearchRepository.findApprovedPostsByTagOrTitle(keyword, 0, 10)
        then:
        searchResult.count { StringUtils.containsIgnoreCase(it.title, keyword) } == acceptedPostsWithGivenKeywordInTitleCount
        searchResult.count { it.approved } == acceptedPostsWithGivenKeywordInTitleCount
    }

    def "should count all approved blog posts containing given keyword in title"() {
        given:
        Blog personalBlog = aBlog(blogType: PERSONAL)
        Blog companyBlog = aBlog(blogType: COMPANY)
        BlogPost lambdaExpressionsInKotlinPost = aBlogPost(title: 'Lambda Expressions in Kotlin', approved: false, blog: personalBlog)
        BlogPost topKotlinLibrariesPost = aBlogPost(title: 'Top Kotlin Libraries', approved: true, blog: personalBlog)
        BlogPost desktopAppInKotlinPost = aBlogPost(title: 'Ktor - backend in Kotlin', approved: true, blog: companyBlog)
        BlogPost kotlinForCSharpDevelopersPost = aBlogPost(title: 'Kotlin for C# developers', approved: true, blog: personalBlog)
        BlogPost introductionToClojurePost = aBlogPost(title: 'Introduction to Clojure', approved: true, blog: personalBlog)

        flushData([personalBlog, companyBlog],
                [],
                [lambdaExpressionsInKotlinPost, topKotlinLibrariesPost, desktopAppInKotlinPost, kotlinForCSharpDevelopersPost, introductionToClojurePost])

        String keyword = 'Kotlin'
        long expectedKotlinBlogPostsCount = 3
        when:
        long kotlinBlogPostsCount = blogPostTextSearchRepository.countApprovedPostsByTagOrTitle(keyword)
        then:
        kotlinBlogPostsCount == expectedKotlinBlogPostsCount
    }

    def "should find approved blog posts containing given keyword in tag"() {
        given:
        Blog personalBlog = aBlog(blogType: PERSONAL)
        Blog companyBlog = aBlog(blogType: COMPANY)
        Tag concurrentProgrammingTag = new Tag('Concurrent Programming')
        Tag jvmTag = new Tag('JVM')
        BlogPost completableFutureBlogPost = aBlogPost(title: 'How to use CompletableFuture', approved: false, blog: personalBlog, tags: [concurrentProgrammingTag])
        BlogPost executorServiceBlogPost = aBlogPost(title: 'Executor Service 101', approved: true, blog: personalBlog, tags: [concurrentProgrammingTag])
        BlogPost akkaBlogPost = aBlogPost(title: 'Akka Introduction', approved: true, blog: companyBlog, tags: [concurrentProgrammingTag, jvmTag])
        BlogPost clojurePost = aBlogPost(title: "Clojure Introduction", approved: true, blog: companyBlog, tags: [jvmTag])

        flushData([personalBlog, companyBlog],
                [concurrentProgrammingTag, jvmTag],
                [completableFutureBlogPost, executorServiceBlogPost, akkaBlogPost, clojurePost]
        )

        String keyword = 'concurrent'
        int acceptedPostsWithGivenTagCount = 2
        when:
        List<BlogPost> searchResult = blogPostTextSearchRepository.findApprovedPostsByTagOrTitle(keyword, 0, 10)
        then:
        searchResult.count { it.tags.collect { it.value }.contains(concurrentProgrammingTag.value) } == acceptedPostsWithGivenTagCount
        searchResult.count { it.approved } == acceptedPostsWithGivenTagCount
    }

    def "should count all approved blog posts by tag"() {
        given:
        Blog personalBlog = aBlog(blogType: PERSONAL)
        Blog companyBlog = aBlog(blogType: COMPANY)
        Tag devopsTag = new Tag('Devops')
        Tag groovyTag = new Tag('Groovy')
        BlogPost introductionToDockerPost = aBlogPost(title: 'Introduction to Docker', approved: false, blog: personalBlog, tags: [devopsTag])
        BlogPost dockerQuickStartPost = aBlogPost(title: 'Docker quick start', approved: true, blog: personalBlog, tags: [devopsTag])
        BlogPost dockerForNonDevopsPost = aBlogPost(title: 'Docker for non Devops ', approved: true, blog: personalBlog, tags: [devopsTag])
        BlogPost dockerComposePost = aBlogPost(title: 'docker-compose 101', approved: true, blog: companyBlog, tags: [devopsTag])
        BlogPost groovyIntroductionPost = aBlogPost(title: 'Groovy Introduction', approved: true, blog: personalBlog, tags: [groovyTag])

        flushData([personalBlog, companyBlog],
                [devopsTag, groovyTag],
                [introductionToDockerPost, dockerQuickStartPost, dockerForNonDevopsPost, dockerComposePost, groovyIntroductionPost]
        )

        String keyword = 'Devops'
        long expectedDevopsBlogPostsCount = 3L
        when:
        long devopsBlogPostsCount = blogPostTextSearchRepository.countApprovedPostsByTagOrTitle(keyword)
        then:
        devopsBlogPostsCount == expectedDevopsBlogPostsCount
    }

    def "should return first page by tag or title and sorting by the published date desc"() {
        given:
        Blog personalBlog = aBlog(blogType: PERSONAL)
        Blog companyBlog = aBlog(blogType: COMPANY)
        Tag springTag = new Tag("Spring")
        Tag devopsTag = new Tag("Devops")
        BlogPost introductionToDockerPost = aBlogPost(title: 'Introduction to Docker', approved: true, blog: personalBlog,
                tags: [devopsTag], publishedDate: LocalDateTime.of(2021, 3, 1, 10, 2))
        BlogPost springAopPost = aBlogPost(title: "Spring AOP explained ", approved: false, blog: personalBlog,
                tags: [springTag], publishedDate: LocalDateTime.of(2021, 3, 1, 10, 2))
        BlogPost springIoCIntroduction = aBlogPost(title: "Custom HealthCheck in Actuator", approved: true, blog: personalBlog,
                tags: [springTag], publishedDate: LocalDateTime.of(2016, 3, 1, 9, 0))
        BlogPost customActuatorHealthCheckPost = aBlogPost(title: "Custom HealthCheck in Actuator", approved: true, blog: personalBlog,
                tags: [springTag], publishedDate: LocalDateTime.of(2021, 3, 1, 10, 2))
        BlogPost transactionsInSpringPost = aBlogPost(title: "Transaction in Spring", approved: true, blog: companyBlog,
                tags: [springTag], publishedDate: LocalDateTime.of(2021, 3, 1, 10, 1))
        BlogPost springDataElasticSearchPost = aBlogPost(title: "Spring Data ElasticSearch", approved: true, blog: companyBlog,
                tags: [springTag], publishedDate: LocalDateTime.of(2021, 3, 1, 9, 20))

        flushData([personalBlog, companyBlog],
                [devopsTag, springTag],
                [introductionToDockerPost, springAopPost, springIoCIntroduction, customActuatorHealthCheckPost, transactionsInSpringPost, springDataElasticSearchPost]
        )

        String keyword = "Spring"
        List<String> expectedFirstThreeTitles = [customActuatorHealthCheckPost.title, transactionsInSpringPost.title, springDataElasticSearchPost.title]
        when:
        List<String> firstThreeTitles = blogPostTextSearchRepository.findApprovedPostsByTagOrTitle(keyword, 0, 3)
                .collect { it.title }
        then:
        firstThreeTitles == expectedFirstThreeTitles
    }

    private flushData(Collection<Blog> blogs, Collection<Tag> tags, Collection<BlogPost> posts) {
        persistCollection(blogs)
        persistCollection(tags)
        persistCollection(posts)
        entityManager.flush()

        def searchSession = Search.session(entityManager)
        searchSession.indexingPlan().execute()
    }

    private persistCollection(Collection entities) {
        for (entity in entities) {
            entityManager.persist(entity)
        }
    }

}
