package pl.tomaszdziurko.jvm_bloggers.newsletter_issues;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.mailing.IssueNumberRetriever;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssueRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.util.List;

@Component
@Slf4j
public class NewsletterIssueService {

    private final MetadataRepository metadataRepository;
    private final IssueNumberRetriever issueNumberRetriever;
    private final NewsletterIssueRepository newsletterIssueRepository;
    private final NowProvider nowProvider;

    @Autowired
    public NewsletterIssueService(MetadataRepository metadataRepository,
                                  IssueNumberRetriever issueNumberRetriever,
                                  NewsletterIssueRepository newsletterIssueRepository,
                                  NowProvider nowProvider) {
        this.metadataRepository = metadataRepository;
        this.issueNumberRetriever = issueNumberRetriever;
        this.newsletterIssueRepository = newsletterIssueRepository;
        this.nowProvider = nowProvider;
    }

    NewsletterIssue saveNewIssue(List<Blog> blogs, List<BlogPost> blogPosts) {
        long nextIssueNumber = issueNumberRetriever.getNextIssueNumber();
        NewsletterIssue newsletterIssue = new NewsletterIssue(
            nextIssueNumber,
            nowProvider.today(),
            blogs,
            blogPosts,
            metadataRepository.findByName(MetadataKeys.HEADING_TEMPLATE).getValue(),
            metadataRepository.findByName(MetadataKeys.VARIA_TEMPLATE).getValue()
        );

        newsletterIssueRepository.save(newsletterIssue);
        log.info("New issue saved: {}" + newsletterIssue);
        return newsletterIssue;
    }

}
