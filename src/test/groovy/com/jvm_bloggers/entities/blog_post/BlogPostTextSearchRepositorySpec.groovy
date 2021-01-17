package com.jvm_bloggers.entities.blog_post

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.tag.Tag
import com.jvm_bloggers.entities.tag.TagRepository
import org.apache.commons.lang3.StringUtils
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.transaction.PlatformTransactionManager
import org.springframework.transaction.TransactionDefinition
import org.springframework.transaction.support.TransactionTemplate

import static com.jvm_bloggers.ObjectMother.aBlog
import static com.jvm_bloggers.ObjectMother.aBlogPost
import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class BlogPostTextSearchRepositorySpec extends SpringContextAwareSpecification {

  @Autowired
  BlogRepository blogRepository

  @Autowired
  TagRepository tagRepository

  @Autowired
  BlogPostRepository blogPostRepository

  @Autowired
  PlatformTransactionManager platformTransactionManager

  def "should find approved blog posts containing given keyword in title"() {
    given:
    Blog personalBlog = aBlog(author: 'title author', blogType: PERSONAL)
    Blog companyBlog = aBlog(author: 'title company', blogType: BlogType.COMPANY)

    Tag functionalProgrammingTag = new Tag('Functional Programming')

    BlogPost javaHttpClient = aBlogPost(title:  'Java 9 HTTP Client', approved: false, blog: personalBlog)
    BlogPost javaConcurrencyBlogPost = aBlogPost(title: 'Java Concurrency', approved: true, blog: personalBlog)
    BlogPost lambdaExpressionsInJavaBlogPost = aBlogPost(title: 'Lambda Expressions in JAVA', approved: true, blog: companyBlog, tags: [functionalProgrammingTag])
    BlogPost parallelStreamsBlogPost = aBlogPost(title: 'Parallel Streams', approved: true, blog: personalBlog, tags: [functionalProgrammingTag])

    createTransactionTemplate().execute({
      blogRepository.saveAll([personalBlog, companyBlog])
      tagRepository.save(functionalProgrammingTag)
      blogPostRepository.saveAll([javaHttpClient, javaConcurrencyBlogPost, lambdaExpressionsInJavaBlogPost, parallelStreamsBlogPost])
    })

    String keyword = 'Java'
    int acceptedPostsWithGivenKeywordInTitleCount = 2

    when:
    List<BlogPost> searchResult = blogPostRepository.findApprovedPostsByTagOrTitle(keyword, 0, 10)
    then:
    searchResult.count { StringUtils.containsIgnoreCase(it.title, keyword) } == acceptedPostsWithGivenKeywordInTitleCount
    searchResult.count {it.approved} == acceptedPostsWithGivenKeywordInTitleCount
  }

  def "should count all approved blog posts containing given keyword in title"() {
    given:
    Blog personalBlog = aBlog(author: 'title another_author', blogType: PERSONAL)
    Blog companyBlog = aBlog(author: 'title another_company', blogType: PERSONAL)
    BlogPost lambdaExpressionsInKotlinPost = aBlogPost(title: 'Lambda Expressions in Kotlin', approved: false, blog: personalBlog)
    BlogPost topKotlinLibrariesPost = aBlogPost(title: 'Top Kotlin Libraries', approved: true, blog: personalBlog)
    BlogPost desktopAppInKotlinPost = aBlogPost(title: 'Ktor - backend in Kotlin', approved: true, blog: companyBlog)
    BlogPost kotlinForCSharpDevelopersPost = aBlogPost(title: 'Kotlin for C# developers', approved: true, blog: personalBlog)
    BlogPost introductionToClojurePost = aBlogPost(title: 'Introduction to Clojure', approved: true, blog: personalBlog)
    createTransactionTemplate().execute({
      blogRepository.saveAll([personalBlog, companyBlog])
      blogPostRepository.saveAll([lambdaExpressionsInKotlinPost, topKotlinLibrariesPost, desktopAppInKotlinPost, kotlinForCSharpDevelopersPost, introductionToClojurePost])
    })
    String keyword = 'Kotlin'
    int expectedKotlinBlogPostsCount = 3
    when:
    int kotlinBlogPostsCount  = blogPostRepository.countApprovedPostsByTagOrTitle(keyword)
    then:
    kotlinBlogPostsCount == expectedKotlinBlogPostsCount

  }


  def "should find approved blog posts containing given keyword in tag"() {
    given:
    Blog personalBlog = aBlog(author: 'tag author', blogType: PERSONAL)
    Blog companyBlog = aBlog(author: 'tag company', blogType: BlogType.COMPANY)

    Tag concurrentProgrammingTag = new Tag('Concurrent Programming')
    Tag jvmTag = new Tag('JVM')

    BlogPost completableFutureBlogPost = aBlogPost(title: 'How to use CompletableFuture', approved: false, blog: personalBlog, tags: [concurrentProgrammingTag])
    BlogPost executorServiceBlogPost = aBlogPost(title: 'Executor Service 101', approved: true, blog: personalBlog, tags: [concurrentProgrammingTag])
    BlogPost akkaBlogPost = aBlogPost(title: 'Akka Introduction', approved: true, blog: companyBlog, tags: [concurrentProgrammingTag, jvmTag])
    BlogPost clojurePost = aBlogPost(title: "Clojure Introduction", approved: true, blog: companyBlog, tags: [jvmTag])

    createTransactionTemplate().execute({
      blogRepository.saveAll([personalBlog, companyBlog])
      tagRepository.saveAll([concurrentProgrammingTag, jvmTag])
      blogPostRepository.saveAll([completableFutureBlogPost, executorServiceBlogPost, akkaBlogPost, clojurePost])
    })

    String keyword = 'concurrent'
    int acceptedPostsWithGivenTagCount = 2

    when:
    List<BlogPost> searchResult = blogPostRepository.findApprovedPostsByTagOrTitle(keyword, 0, 10)

    then:
    searchResult.count {it.tags.collect {it.value}.contains(concurrentProgrammingTag.value)} == acceptedPostsWithGivenTagCount
    searchResult.count {it.approved} == acceptedPostsWithGivenTagCount
  }

  def "should count all approved blog posts by tag" () {
    given:
    Blog personalBlog = aBlog(author: 'tag another_author', blogType: PERSONAL)
    Blog companyBlog = aBlog(author: 'tag another_company', blogType: COMPANY)

    Tag devopsTag = new Tag('Devops')
    Tag groovyTag = new Tag('Groovy')

    BlogPost introductionToDockerPost = aBlogPost(title: 'Introduction to Docker', approved: false, blog: personalBlog, tags: [devopsTag])
    BlogPost dockerQuickStartPost = aBlogPost(title: 'Docker quick start', approved: true, blog: personalBlog, tags: [devopsTag])
    BlogPost dockerForNonDevopsPost = aBlogPost(title: 'Docker for non Devops ', approved: true, blog: personalBlog, tags: [devopsTag])
    BlogPost dockerComposePost = aBlogPost(title: 'docker-compose 101', approved: true, blog: companyBlog, tags: [devopsTag])
    BlogPost groovyIntroductionPost = aBlogPost(title: 'Groovy Introduction', approved: true, blog: personalBlog, tags: [groovyTag])

    createTransactionTemplate().execute({
      blogRepository.saveAll([personalBlog, companyBlog])
      tagRepository.saveAll([devopsTag, groovyTag])
      blogPostRepository.saveAll([introductionToDockerPost, dockerQuickStartPost, dockerForNonDevopsPost, dockerComposePost, groovyIntroductionPost])
    })

    String keyword = 'Devops'
    int expectedDevopsBlogPostsCount = 3
    when:
    int devopsBlogPostsCount  = blogPostRepository.countApprovedPostsByTagOrTitle(keyword)
    then:
    devopsBlogPostsCount == expectedDevopsBlogPostsCount
  }


  private TransactionTemplate createTransactionTemplate() {
    TransactionTemplate transactionTemplate = new TransactionTemplate(platformTransactionManager)
    transactionTemplate.propagationBehavior = TransactionDefinition.PROPAGATION_REQUIRES_NEW
    return transactionTemplate
  }
}
