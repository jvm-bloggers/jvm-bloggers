package com.jvm_bloggers.domain.command.top_posts_summary;

import com.jvm_bloggers.domain.command.CommandHandler;
import com.jvm_bloggers.domain.query.MostPopularBlogPostsQuery;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.click.PostIdWithCount;
import com.jvm_bloggers.entities.top_posts_summary.PopularCompanyPost;
import com.jvm_bloggers.entities.top_posts_summary.PopularPersonalPost;
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummary;
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummaryRepository;
import com.jvm_bloggers.utils.NowProvider;
import io.vavr.collection.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.YearMonth;

import static com.jvm_bloggers.utils.DateTimeUtilities.DAY_OF_MONTH_ENDING_SUMMARY_PERIOD;

@Service
@Slf4j
@RequiredArgsConstructor
class GenerateTopPostsInMonthSummaryCommandHandler
    implements CommandHandler<GenerateTopPostsInMonthSummary> {

    private final MostPopularBlogPostsQuery query;
    private final TopPostsSummaryRepository topPostsSummaryRepository;
    private final BlogPostRepository blogPostRepository;
    private final NowProvider nowProvider;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    @EventListener
    @Transactional
    public void handle(GenerateTopPostsInMonthSummary command) {

        LocalDateTime startOfPreviousMonth = calculateStartDate(command.getYearMonth());
        LocalDateTime tenthDayOfCurrentMonth = calculateEndDate(command.getYearMonth());
        List<PopularPersonalPost> personalPosts = createPopularPersonalBlogPosts(
            command,
            startOfPreviousMonth,
            tenthDayOfCurrentMonth
        );
        List<PopularCompanyPost> companyPosts = createPopularCompanyPosts(
            command,
            startOfPreviousMonth,
            tenthDayOfCurrentMonth
        );

        TopPostsSummary topPostsSummary = new TopPostsSummary(
            command.getYearMonth().getYear(),
            command.getYearMonth().getMonthValue(),
            nowProvider.now(),
            personalPosts.toJavaList(),
            companyPosts.toJavaList()
        );
        topPostsSummaryRepository.save(topPostsSummary);
        eventPublisher.publishEvent(new TopPostsSummaryGenerated(command.getYearMonth()));
    }

    private LocalDateTime calculateStartDate(YearMonth yearMonth) {
        return yearMonth
            .atDay(1)
            .atStartOfDay();
    }

    private LocalDateTime calculateEndDate(YearMonth yearMonth) {
        return yearMonth
            .atDay(DAY_OF_MONTH_ENDING_SUMMARY_PERIOD)
            .plusMonths(1)
            .atStartOfDay();
    }

    private List<PopularPersonalPost> createPopularPersonalBlogPosts(
        GenerateTopPostsInMonthSummary command,
        LocalDateTime startOfPreviousMonth,
        LocalDateTime tenthDayOfCurrentMonth
    ) {
        List<PostIdWithCount> bestPersonalPosts = query.getBestPersonalPosts(
            startOfPreviousMonth,
            tenthDayOfCurrentMonth,
            command.getNumberOfPersonalPosts()
        );

        return bestPersonalPosts
            .zipWithIndex()
            .map(postWithIndex -> {
                long position = (long) postWithIndex._2 + 1;
                PostIdWithCount postIdWithCount = postWithIndex._1;
                BlogPost post = blogPostRepository.getOne(postIdWithCount.getBlogPostId());
                return new PopularPersonalPost(post, position, postIdWithCount.getCount());
            });
    }

    private List<PopularCompanyPost> createPopularCompanyPosts(
        GenerateTopPostsInMonthSummary command,
        LocalDateTime startOfPreviousMonth,
        LocalDateTime tenthDayOfCurrentMonth
    ) {
        List<PostIdWithCount> bestCompanyPosts = query.getBestCompanyPosts(
            startOfPreviousMonth,
            tenthDayOfCurrentMonth,
            command.getNumberOfCompanyPosts()
        );

        return bestCompanyPosts
            .zipWithIndex()
            .map(postWithIndex -> {
                long position = (long) postWithIndex._2 + 1;
                PostIdWithCount postIdWithCount = postWithIndex._1;
                BlogPost post = blogPostRepository.getOne(postIdWithCount.getBlogPostId());
                return new PopularCompanyPost(post, position, postIdWithCount.getCount());
            });
    }

}
