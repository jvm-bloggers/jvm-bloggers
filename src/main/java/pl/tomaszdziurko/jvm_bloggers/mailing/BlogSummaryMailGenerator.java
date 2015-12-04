package pl.tomaszdziurko.jvm_bloggers.mailing;


import com.google.common.base.Joiner;
import com.sun.syndication.feed.synd.SyndFeed;
import lombok.extern.slf4j.Slf4j;
import org.antlr.stringtemplate.StringTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
@Slf4j
public class BlogSummaryMailGenerator {

    private final Resource blogsSummaryTemplate;
    private final SyndFeedProducer syndFeedFactory;

    @Autowired
    public BlogSummaryMailGenerator(@Value("classpath:mail_templates/blog_summary.st") Resource blogsSummaryTemplate,
                                    SyndFeedProducer syndFeedFactory) {
        this.blogsSummaryTemplate = blogsSummaryTemplate;
        this.syndFeedFactory = syndFeedFactory;
    }

    public String generateSummaryMail(List<BlogPost> postsFromPersonalBlogs,
                                      List<BlogPost> postsFromCompanies,
                                      List<Blog> blogsAddedSinceLastNewsletter, int numberOfDaysBackInThePast) {
        try {
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(blogsSummaryTemplate.getInputStream(), "UTF-8"));
            String templateContent =  Joiner.on("\n").join(bufferedReader.lines().collect(Collectors.toList()));
            StringTemplate template = new StringTemplate(templateContent);
            template.setAttribute("days", numberOfDaysBackInThePast);
            template.setAttribute("newPosts", postsFromPersonalBlogs.stream().map(BlogPostForMailItem::new).collect(Collectors.toList()));
            template.setAttribute("newPostsFromCompanies", postsFromCompanies.stream().map(BlogPostForMailItem::new).collect(Collectors.toList()));
            template.setAttribute("blogsWithHomePage", getBlogAndItsHomepage(blogsAddedSinceLastNewsletter));
            return template.toString();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
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
