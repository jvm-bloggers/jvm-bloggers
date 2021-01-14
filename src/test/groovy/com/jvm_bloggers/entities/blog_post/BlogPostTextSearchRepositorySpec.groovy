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

  def "should find blog posts containing given keyword in title"() {
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


  def "should find blog posts containing given keyword in tag"() {
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


  private TransactionTemplate createTransactionTemplate() {
    TransactionTemplate transactionTemplate = new TransactionTemplate(platformTransactionManager)
    transactionTemplate.propagationBehavior = TransactionDefinition.PROPAGATION_REQUIRES_NEW
    return transactionTemplate
  }
}
