package com.jvm_bloggers.schedulers.top_posts_summary;

import com.jvm_bloggers.domain.command.CommandPublisher;
import com.jvm_bloggers.domain.command.top_posts_summary.GenerateTopPostsInMonthSummary;
import com.jvm_bloggers.utils.NowProvider;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.YearMonth;

import static com.jvm_bloggers.utils.ZoneTimeProvider.DEFAULT_ZONE_NAME;

@Service
@RequiredArgsConstructor
public class TopPostsInPreviousMonthSummaryScheduler {

    private static final int NUMBER_OF_TOP_PERSONAL_POSTS = 10;
    private static final int NUMBER_OF_TOP_COMPANY_POSTS = 5;

    private final CommandPublisher commandPublisher;
    private final NowProvider nowProvider;

    @Scheduled(cron = "${scheduler.top-posts-in-month-summary}", zone = DEFAULT_ZONE_NAME)
    public void publishTopPostsInPreviousMonth() {

        YearMonth yearMonth = YearMonth.from(nowProvider.today().minusMonths(1));

        commandPublisher.publish(
            new GenerateTopPostsInMonthSummary(
                yearMonth,
                NUMBER_OF_TOP_PERSONAL_POSTS,
                NUMBER_OF_TOP_COMPANY_POSTS
            )
        );

    }

}
