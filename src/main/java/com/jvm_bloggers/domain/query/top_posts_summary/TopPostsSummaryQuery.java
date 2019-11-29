package com.jvm_bloggers.domain.query.top_posts_summary;

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedPost;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.top_posts_summary.PopularCompanyPost;
import com.jvm_bloggers.entities.top_posts_summary.PopularPersonalPost;
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummary;
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummaryRepository;
import io.vavr.collection.List;
import io.vavr.control.Option;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.YearMonth;

import static org.springframework.data.domain.Sort.Direction.DESC;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class TopPostsSummaryQuery {

    private final TopPostsSummaryRepository repository;
    private final LinkGenerator linkGenerator;

    public List<TopPostsSummaryBasicDetails> loadBasicDetailsForAllSummaries() {
        List<TopPostsSummary> allSummaries = List
            .ofAll(repository.findAll(Sort.by(DESC, "year", "month")));
        return allSummaries.map(s -> new TopPostsSummaryBasicDetails(s.getYearMonth()));
    }

    public Option<PublishedTopPostSummary> findFor(YearMonth yearMonth) {
        return repository.findOneByYearAndMonth(yearMonth.getYear(), yearMonth.getMonthValue())
            .map(s -> {
                List<PublishedPost> topPersonalPosts =
                    convertPersonalPosts(s.getPopularPersonalPosts());
                List<PublishedPost> topCompanyPosts =
                    convertCompanyPosts(s.getPopularCompanyPosts());
                return new PublishedTopPostSummary(
                    s.getYearMonth(),
                    topPersonalPosts,
                    topCompanyPosts
                );
            });
    }

    private List<PublishedPost> convertPersonalPosts(java.util.List<PopularPersonalPost> posts) {
        return List
            .ofAll(posts)
            .map(PopularPersonalPost::getBlogPost)
            .map(this::convert);
    }

    private List<PublishedPost> convertCompanyPosts(java.util.List<PopularCompanyPost> posts) {
        return List
            .ofAll(posts)
            .map(PopularCompanyPost::getBlogPost)
            .map(this::convert);
    }

    private PublishedPost convert(BlogPost blogPost) {
        return PublishedPost.builder()
            .url(linkGenerator.generateRedirectLinkFor(blogPost.getUid()))
            .title(blogPost.getTitle())
            .authorName(blogPost.getBlog().getAuthor())
            .authorTwitterHandle(blogPost.getBlog().getTwitter())
            .build();
    }

}
