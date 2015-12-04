package pl.tomaszdziurko.jvm_bloggers.mailing;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddress;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddressRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Collections.EMPTY_LIST;

@Component
@Slf4j
public class BlogSummaryMailSender {

    public static final String MAIL_SUMMARY_TITLE_PREFIX = "[JVM Bloggers] #";
    public static final String MAIL_SUMMARY_TITLE_POSTIFX = ": Nowe wpisy na polskich blogach, ";
    public static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    private final BlogPostRepository blogPostRepository;
    private final BlogRepository blogRepository;
    private final BlogSummaryMailGenerator mailGenerator;
    private final MailSender mailSender;
    private final MailingAddressRepository mailingAddressRepository;
    private final IssueNumberRetriever issueNumberRetriever;
    private final NowProvider nowProvider;

    @Autowired
    public BlogSummaryMailSender(BlogPostRepository blogPostRepository,
                                 BlogRepository blogRepository,
                                 BlogSummaryMailGenerator blogSummaryMailGenerator,
                                 MailSender sendGridMailSender,
                                 MailingAddressRepository mailingAddressRepository,
                                 IssueNumberRetriever issueNumberRetriever,
                                 NowProvider nowProvider) {
        this.blogPostRepository = blogPostRepository;
        this.blogRepository = blogRepository;
        this.mailGenerator = blogSummaryMailGenerator;
        this.mailSender = sendGridMailSender;
        this.mailingAddressRepository = mailingAddressRepository;
        this.issueNumberRetriever = issueNumberRetriever;
        this.nowProvider = nowProvider;
    }

    public void sendSummary(int numberOfDaysBackInThePast) {
        LocalDateTime publishedDate = nowProvider.now().minusDays(numberOfDaysBackInThePast).withHour(14).withMinute(20).withSecond(0).withNano(0);
        List<Blog> blogsAddedSinceLastNewsletter = blogRepository.findByDateAddedAfter(publishedDate);
        List<BlogPost> newApprovedPosts = blogPostRepository.findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(publishedDate);
        if (newApprovedPosts.isEmpty() && blogsAddedSinceLastNewsletter.isEmpty()) {
            log.warn("There are no new posts nor new blogs added for last {} days !!!", numberOfDaysBackInThePast);
            return;
        }

        Map<BlogType, List<BlogPost>> newBlogPostsByType = newApprovedPosts.stream().collect(Collectors.groupingBy(it -> it.getBlog().getBlogType()));

        List<MailingAddress> mailingAddresses = mailingAddressRepository.findAll();
        if (mailingAddresses.isEmpty()) {
            log.warn("No e-mails in database to send Blog Summary !!!");
            return;
        }

        List<BlogPost> newPostsFromPersonalBlogs = newBlogPostsByType.getOrDefault(BlogType.PERSONAL, EMPTY_LIST);
        List<BlogPost> newPostsfromCompanies = newBlogPostsByType.getOrDefault(BlogType.COMPANY, EMPTY_LIST);
        String mailTemplate = mailGenerator.generateSummaryMail(newPostsFromPersonalBlogs,
            newPostsfromCompanies, blogsAddedSinceLastNewsletter, numberOfDaysBackInThePast);
        log.info("Mail content = \n" + mailTemplate);
        String issueTitle = prepareIssueTitle();
        mailingAddresses.stream().map(MailingAddress::getAddress).forEach(recipient -> {
                mailSender.sendEmail(recipient, issueTitle, mailTemplate);
            }
        );

    }

    private String prepareIssueTitle() {
        long issueNumber = issueNumberRetriever.getNextIssueNumber();
        return MAIL_SUMMARY_TITLE_PREFIX + issueNumber + MAIL_SUMMARY_TITLE_POSTIFX + getTodayDateAsString();
    }

    private String getTodayDateAsString() {
        return nowProvider.now().format(FORMATTER);
    }
}
