package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.domain.command.CommandPublisher;
import com.jvm_bloggers.domain.command.top_posts_summary.GenerateTopPostsInMonthSummary;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.utils.NowProvider;
import io.vavr.collection.Stream;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.YearMonth;
import java.util.List;

import static com.jvm_bloggers.utils.DateTimeUtilities.DAY_OF_MONTH_ENDING_SUMMARY_PERIOD;
import static com.jvm_bloggers.utils.DateTimeUtilities.lastPublicationDate;

@Service
@RequiredArgsConstructor
public class AdminDashboardPageBackingBean {

    private static int NUMBER_OF_MONTHS_TO_GENERATE_TOP_POSTS_SUMMARY = 6;

    private final NowProvider nowProvider;
    private final CommandPublisher commandPublisher;
    private final BlogPostRepository blogPostRepository;

    public void generateTopPostsSummary(YearMonth yearMonth) {
        commandPublisher.publish(
            new GenerateTopPostsInMonthSummary(yearMonth, 10, 5)
        );
    }

    public List<YearMonth> prepareYearMonthChoices() {
        LocalDateTime now = nowProvider.now();
        YearMonth start = calculateStart(now);
        return Stream
            .range(0, NUMBER_OF_MONTHS_TO_GENERATE_TOP_POSTS_SUMMARY)
            .map(start::minusMonths)
            .toJavaList();
    }

    private YearMonth calculateStart(LocalDateTime now) {
        if (now.getDayOfMonth() >= DAY_OF_MONTH_ENDING_SUMMARY_PERIOD) {
            return YearMonth.from(now.minusMonths(1));
        } else {
            return YearMonth.from(now.minusMonths(2));
        }
    }

    public Integer getNumberOfPostsSinceLastPublication() {
        LocalDateTime lastPublicationDate = lastPublicationDate(nowProvider.now());
        return blogPostRepository.countByPublishedDateAfter(lastPublicationDate);
    }

    public Integer getNumberOfPostsWaitingForModeration() {
        return blogPostRepository.countByApprovedIsNull();
    }

}
