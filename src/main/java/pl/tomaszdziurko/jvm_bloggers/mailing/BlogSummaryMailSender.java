package pl.tomaszdziurko.jvm_bloggers.mailing;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.time.LocalDateTime;
import java.util.List;

@Component
@Slf4j
public class BlogSummaryMailSender {

    private BlogPostRepository blogPostRepository;
    private BlogSummaryMailGenerator mailGenerator;
    private NowProvider nowProvider;

    @Autowired
    public BlogSummaryMailSender(BlogPostRepository blogPostRepository, BlogSummaryMailGenerator blogSummaryMailGenerator, NowProvider nowProvider) {
        this.blogPostRepository = blogPostRepository;
        this.mailGenerator = blogSummaryMailGenerator;
        this.nowProvider = nowProvider;
    }

    public void sendSummary(int numberOfDaysBackInThePast) {
        LocalDateTime publishedDate = nowProvider.now().minusDays(numberOfDaysBackInThePast).withHour(0).withMinute(0).withSecond(0).withNano(0);
        List<BlogPost> newBlogPosts = blogPostRepository.findByPublishedDateAfterOrderByPublishedDateAsc(publishedDate);
        String mailTemplate = mailGenerator.generateSummaryMail(newBlogPosts, numberOfDaysBackInThePast);
        log.info("Mail content = \n" + mailTemplate);
    }
}
