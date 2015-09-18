package pl.tomaszdziurko.jvm_bloggers.mailing;


import com.google.common.base.Joiner;
import com.google.common.io.Files;
import lombok.extern.slf4j.Slf4j;
import org.antlr.stringtemplate.StringTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.stream.Collectors;

@Component
@Slf4j
public class BlogSummaryMailGenerator {

    private Resource blogsSummaryTemplate;

    @Autowired
    public BlogSummaryMailGenerator(@Value("classpath:/mail_templates/blog_summary.st") Resource blogsSummaryTemplate) {
        this.blogsSummaryTemplate = blogsSummaryTemplate;
    }

    public String generateSummaryMail(List<BlogPost> posts, int numberOfDaysBackInThePast) {
        try {
            List<String> lines = Files.readLines(blogsSummaryTemplate.getFile(), Charset.forName("UTF-8"));
            String templateContent = Joiner.on("\n").join(lines);
            StringTemplate template = new StringTemplate(templateContent);
            template.setAttribute("days", numberOfDaysBackInThePast);
            template.setAttribute("newPosts", posts.stream().map(BlogPostForMailItem::new).collect(Collectors.toList()));
            return template.toString();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


}
