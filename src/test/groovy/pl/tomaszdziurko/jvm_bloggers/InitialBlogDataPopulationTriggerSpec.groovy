package pl.tomaszdziurko.jvm_bloggers

import pl.tomaszdziurko.jvm_bloggers.blog_posts.BlogPostsFetchingScheduler;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository
import pl.tomaszdziurko.jvm_bloggers.blogs.BloggersDataFetchingScheduler;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import spock.lang.Specification;
import spock.lang.Subject

class InitialBlogDataPopulationTriggerSpec extends Specification {

    BlogRepository blogRepository = Mock()
    BlogPostRepository blogPostRepository = Mock()
    BloggersDataFetchingScheduler bloggersDataFetchingScheduler = Mock();
    BlogPostsFetchingScheduler blogPostsFetchingScheduler = Mock();

    @Subject
    InitialBlogDataPopulationTrigger tested = new InitialBlogDataPopulationTrigger(blogRepository, blogPostRepository, bloggersDataFetchingScheduler, blogPostsFetchingScheduler)

    def "Should trigger bloggers data population if there is no data"() {
        given:
            1 * blogRepository.count() >> 0
            1 * blogPostRepository.count() >> 0
        when:
            tested.initializeDatabaseWithBlogDataIfEmpty()
        then:
            1 * bloggersDataFetchingScheduler.fetchBloggersData();
            1 * blogPostsFetchingScheduler.checkRssForNewBlogPosts();
    }

    def "Should skip if bloggers data already exists"() {
        given:
            1 * blogRepository.count() >> 1
            1 * blogPostRepository.count() >> 1
        when:
            tested.initializeDatabaseWithBlogDataIfEmpty()
        then:
            0 * bloggersDataFetchingScheduler.fetchBloggersData();
            0 * blogPostsFetchingScheduler.checkRssForNewBlogPosts();
    }

}
