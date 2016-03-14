package pl.tomaszdziurko.jvm_bloggers.mailing;

import com.sun.syndication.feed.synd.SyndFeed;

import lombok.extern.slf4j.Slf4j;

import org.antlr.stringtemplate.StringTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;
import pl.tomaszdziurko.jvm_bloggers.settings.Setting;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingKeys;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;
import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;

@Component
@Slf4j
public class BlogSummaryMailGenerator {

    private BlogRepository blogRepository;
    private BlogPostRepository blogPostRepository;
    private SettingRepository settingRepository;
    private NowProvider nowProvider;
    private SyndFeedProducer syndFeedFactory;

    // This constructor exists only to make Wicket @SpringBean work for Spring beans
    // with constructor injection using @Autowired
    public BlogSummaryMailGenerator() {

    }

    @Autowired
    public BlogSummaryMailGenerator(BlogRepository blogRepository,
                                    BlogPostRepository blogPostRepository,
                                    SettingRepository settingRepository,
                                    NowProvider nowProvider,
                                    SyndFeedProducer syndFeedFactory) {
        this.blogRepository = blogRepository;
        this.blogPostRepository = blogPostRepository;
        this.settingRepository = settingRepository;
        this.nowProvider = nowProvider;
        this.syndFeedFactory = syndFeedFactory;
    }

    public String prepareMailContent(int numberOfDaysBackInThePast, long issueNumber) {
        LocalDateTime publishedDate = nowProvider.now().minusDays(numberOfDaysBackInThePast)
            .withHour(11)
            .withMinute(0)
            .withSecond(0).withNano(0);
        List<Blog> blogsAddedSinceLastNewsletter =
            blogRepository.findByDateAddedAfter(publishedDate);
        List<BlogPost> newApprovedPosts = blogPostRepository
                .findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(publishedDate);
        if (newApprovedPosts.isEmpty() && blogsAddedSinceLastNewsletter.isEmpty()) {
            log.warn("There are no new posts nor new blogs added for last {} days !!!",
                numberOfDaysBackInThePast);
        }

        Map<BlogType, List<BlogPost>> newBlogPostsByType = newApprovedPosts
            .stream()
            .collect(Collectors.groupingBy(it -> it.getBlog().getBlogType()));

        List<BlogPost> newPostsFromPersonalBlogs =
            newBlogPostsByType.getOrDefault(BlogType.PERSONAL, emptyList());
        List<BlogPost> newPostsfromCompanies =
            newBlogPostsByType.getOrDefault(BlogType.COMPANY, emptyList());

        Setting mailingTemplate = settingRepository
            .findByName(SettingKeys.MAILING_TEMPLATE.toString());
        String templateContent = mailingTemplate.getValue();
        StringTemplate template = new StringTemplate(templateContent);
        template.setAttribute("days", numberOfDaysBackInThePast);
        template.setAttribute("newPosts", toMailItems(newPostsFromPersonalBlogs, issueNumber));
        template.setAttribute("newPostsFromCompanies",
            toMailItems(newPostsfromCompanies, issueNumber));
        template.setAttribute("blogsWithHomePage",
            getBlogAndItsHomepage(blogsAddedSinceLastNewsletter));
        return template.toString();
    }

    private List<BlogPostForMailItem> toMailItems(List<BlogPost> newPosts, long issueNumber) {
        return newPosts.stream().map(blogPost ->
            BlogPostForMailItem.builder()
                .from(blogPost)
                .withIssueNumber(issueNumber)
                .withDefaultUtmParameters()
                .build()
        ).collect(Collectors.toList());
    }

    private Map<Blog, String> getBlogAndItsHomepage(List<Blog> blogsAddedSinceLastNewsletter) {
        return blogsAddedSinceLastNewsletter.stream().collect(Collectors.toMap(
            Function.identity(),
            blog -> getBlogHomepageFromRss(blog.getRss()))
        );
    }

    private String getBlogHomepageFromRss(String rss) {
        SyndFeed syndFeed = syndFeedFactory.createFor(rss);
        return syndFeed.getLink();
    }

}
