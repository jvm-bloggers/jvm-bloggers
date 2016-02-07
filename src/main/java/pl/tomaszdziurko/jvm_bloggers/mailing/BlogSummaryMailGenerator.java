package pl.tomaszdziurko.jvm_bloggers.mailing;


import com.sun.syndication.feed.synd.SyndFeed;
import lombok.extern.slf4j.Slf4j;
import org.antlr.stringtemplate.StringTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.settings.Setting;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingKeys;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
@Slf4j
public class BlogSummaryMailGenerator {

    private final SyndFeedProducer syndFeedFactory;
    private final SettingRepository settingRepository;

    @Autowired
    public BlogSummaryMailGenerator(SettingRepository settingRepository,
                                    SyndFeedProducer syndFeedFactory) {
        this.settingRepository = settingRepository;
        this.syndFeedFactory = syndFeedFactory;
    }

    public String generateSummaryMail(List<BlogPost> postsFromPersonalBlogs,
                                      List<BlogPost> postsFromCompanies,
                                      List<Blog> blogsAddedSinceLastNewsletter, int numberOfDaysBackInThePast) {
        Setting mailingTemplate = settingRepository.findByName(SettingKeys.MAILING_TEMPLATE.toString());
        String templateContent =  mailingTemplate.getValue();
        StringTemplate template = new StringTemplate(templateContent);
        template.setAttribute("days", numberOfDaysBackInThePast);
        template.setAttribute("newPosts", postsFromPersonalBlogs.stream().map(BlogPostForMailItem::new).collect(Collectors.toList()));
        template.setAttribute("newPostsFromCompanies", postsFromCompanies.stream().map(BlogPostForMailItem::new).collect(Collectors.toList()));
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
