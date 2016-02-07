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
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddress;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddressRepository;
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

import static java.util.Collections.EMPTY_LIST;

@Component
@Slf4j
public class BlogSummaryMailGenerator {

    private final SyndFeedProducer syndFeedFactory;
    private final SettingRepository settingRepository;
    private final BlogRepository blogRepository;
    private final BlogPostRepository blogPostRepository;
    private final NowProvider nowProvider;

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

    public String prepareMailContent(int numberOfDaysBackInThePast) {
        LocalDateTime publishedDate = nowProvider.now().minusDays(numberOfDaysBackInThePast).withHour(11).withMinute(00).withSecond(0).withNano(0);
        List<Blog> blogsAddedSinceLastNewsletter = blogRepository.findByDateAddedAfter(publishedDate);
        List<BlogPost> newApprovedPosts = blogPostRepository.findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(publishedDate);
        if (newApprovedPosts.isEmpty() && blogsAddedSinceLastNewsletter.isEmpty()) {
            log.warn("There are no new posts nor new blogs added for last {} days !!!", numberOfDaysBackInThePast);
        }

        Map<BlogType, List<BlogPost>> newBlogPostsByType = newApprovedPosts.stream().collect(Collectors.groupingBy(it -> it.getBlog().getBlogType()));

        List<BlogPost> newPostsFromPersonalBlogs = newBlogPostsByType.getOrDefault(BlogType.PERSONAL, EMPTY_LIST);
        List<BlogPost> newPostsfromCompanies = newBlogPostsByType.getOrDefault(BlogType.COMPANY, EMPTY_LIST);

        Setting mailingTemplate = settingRepository.findByName(SettingKeys.MAILING_TEMPLATE.toString());
        String templateContent =  mailingTemplate.getValue();
        StringTemplate template = new StringTemplate(templateContent);
        template.setAttribute("days", numberOfDaysBackInThePast);
        template.setAttribute("newPosts", newPostsFromPersonalBlogs.stream().map(BlogPostForMailItem::new).collect(Collectors.toList()));
        template.setAttribute("newPostsFromCompanies", newPostsfromCompanies.stream().map(BlogPostForMailItem::new).collect(Collectors.toList()));
        template.setAttribute("blogsWithHomePage", getBlogAndItsHomepage(blogsAddedSinceLastNewsletter));
        return template.toString();
    }

    private Map<Blog, String> getBlogAndItsHomepage(List<Blog> blogsAddedSinceLastNewsletter) {
        return blogsAddedSinceLastNewsletter.stream().collect(Collectors.toMap(
                        Function.identity(),
                        blog->getBlogHomepageFromRss(blog.getRss()))
        );
    }

    private String getBlogHomepageFromRss(String rss) {
        SyndFeed syndFeed = syndFeedFactory.createFor(rss);
        return syndFeed.getLink();
    }

}
