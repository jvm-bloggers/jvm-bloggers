package pl.tomaszdziurko.jvm_bloggers.mailing;


import com.google.common.base.Joiner;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;
import lombok.extern.slf4j.Slf4j;
import org.antlr.stringtemplate.StringTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.people.domain.Person;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
@Slf4j
public class BlogSummaryMailGenerator {

    private final Resource blogsSummaryTemplate;

    @Autowired
    public BlogSummaryMailGenerator(@Value("classpath:mail_templates/blog_summary.st") Resource blogsSummaryTemplate) {
        this.blogsSummaryTemplate = blogsSummaryTemplate;
    }

    public String generateSummaryMail(List<BlogPost> posts, List<Person> blogsAddedSinceLastNewsletter, int numberOfDaysBackInThePast) {
        try {
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(blogsSummaryTemplate.getInputStream(), "UTF-8"));
            String templateContent =  Joiner.on("\n").join(bufferedReader.lines().collect(Collectors.toList()));
            StringTemplate template = new StringTemplate(templateContent);
            template.setAttribute("days", numberOfDaysBackInThePast);
            template.setAttribute("newPosts", posts.stream().map(BlogPostForMailItem::new).collect(Collectors.toList()));
            template.setAttribute("personToBlogHomepage", getPersonToBlogHomepage(blogsAddedSinceLastNewsletter));
            return template.toString();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Map<Person, String> getPersonToBlogHomepage(List<Person> blogsAddedSinceLastNewsletter) {
        SyndFeedInput syndFeedInput = new SyndFeedInput();
        Map<Person, String> personToBlogHomepage = blogsAddedSinceLastNewsletter.stream().collect(Collectors.toMap(
                        Function.identity(),
                        person->getBlogHomepageFromRss(person.getRss(), syndFeedInput))
        );

        return personToBlogHomepage;
    }

    private String getBlogHomepageFromRss(String rss, SyndFeedInput syndFeedInput){
        String homePageUrl = "";
        try {
            SyndFeed feed = syndFeedInput.build(new XmlReader(new URL(rss)));
            homePageUrl = feed.getLink();
        } catch (FeedException| IOException e) {
            e.printStackTrace();
        }
        return homePageUrl;
    }

}
