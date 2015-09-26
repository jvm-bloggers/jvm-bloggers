package pl.tomaszdziurko.jvm_bloggers.mailing;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.people.Person;
import pl.tomaszdziurko.jvm_bloggers.people.PersonRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Component
@Slf4j
public class BlogSummaryMailSender {

    public static final String MAIL_SUMMARY_TITLE = "[JVM Bloggers] Nowe wpisy na polskich blogach, ";
    public static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    private final BlogPostRepository blogPostRepository;
    private final PersonRepository personRepository;
    private final BlogSummaryMailGenerator mailGenerator;
    private final SendGridMailSender mailSender;
    private final NewsletterRecipientsProvider newsletterRecipientsProvider;
    private final NowProvider nowProvider;

    @Autowired
    public BlogSummaryMailSender(BlogPostRepository blogPostRepository,
                                 PersonRepository personRepository,
                                 BlogSummaryMailGenerator blogSummaryMailGenerator,
                                 SendGridMailSender sendGridMailSender,
                                 NewsletterRecipientsProvider newsletterRecipientsProvider,
                                 NowProvider nowProvider) {
        this.blogPostRepository = blogPostRepository;
        this.personRepository = personRepository;
        this.mailGenerator = blogSummaryMailGenerator;
        this.mailSender = sendGridMailSender;
        this.newsletterRecipientsProvider = newsletterRecipientsProvider;
        this.nowProvider = nowProvider;
    }

    public void sendSummary(int numberOfDaysBackInThePast) {
        LocalDateTime publishedDate = nowProvider.now().minusDays(numberOfDaysBackInThePast).withHour(0).withMinute(0).withSecond(0).withNano(0);
        List<Person> blogsAddedSinceLastNewsletter = personRepository.findByDateAddedAfter(publishedDate);
        List<BlogPost> newBlogPosts = blogPostRepository.findByPublishedDateAfterOrderByPublishedDateAsc(publishedDate);
        String mailTemplate = mailGenerator.generateSummaryMail(newBlogPosts, blogsAddedSinceLastNewsletter, numberOfDaysBackInThePast);
        log.info("Mail content = \n" + mailTemplate);
        newsletterRecipientsProvider.getRecipients().stream().forEach(recipient ->
            mailSender.sendEmail(recipient, MAIL_SUMMARY_TITLE + getTodayDateAsString(), mailTemplate)
        );

    }

    private String getTodayDateAsString() {
        return nowProvider.now().format(FORMATTER);
    }
}
