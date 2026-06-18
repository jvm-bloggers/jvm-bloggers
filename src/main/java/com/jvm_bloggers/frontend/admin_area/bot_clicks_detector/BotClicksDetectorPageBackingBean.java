package com.jvm_bloggers.frontend.admin_area.bot_clicks_detector;

import com.jvm_bloggers.domain.query.BlogPostsClicksCounterQuery;
import com.jvm_bloggers.entities.click.PostClicksCountByIpAddress;
import com.jvm_bloggers.entities.click.PostClicksCountByUserAgent;
import com.jvm_bloggers.utils.NowProvider;

import io.vavr.collection.List;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

import java.time.LocalTime;

import static java.time.temporal.TemporalAdjusters.firstDayOfMonth;
import static java.time.temporal.TemporalAdjusters.lastDayOfMonth;

@Service
@RequiredArgsConstructor
public class BotClicksDetectorPageBackingBean {

    private static final int TOP_RESULTS_SIZE = 10;

    private final NowProvider nowProvider;
    private final BlogPostsClicksCounterQuery botClicksDetectorQuery;

    public List<PostClicksCountByIpAddress> getTop10PostsClicksFromSameIpForPreviousMonth() {
        return botClicksDetectorQuery.topPostsClicksCountByIpAddress(
            nowProvider.today().minusMonths(1).with(firstDayOfMonth()).atStartOfDay(),
            nowProvider.today().minusMonths(1).with(lastDayOfMonth()).atTime(LocalTime.MAX),
            TOP_RESULTS_SIZE
        );
    }

    public List<PostClicksCountByUserAgent> getTop10PostsClicksFromSameUserAgentForPreviousMonth() {
        return botClicksDetectorQuery.topPostsClicksCountByUserAgent(
            nowProvider.today().minusMonths(1).with(firstDayOfMonth()).atStartOfDay(),
            nowProvider.today().minusMonths(1).with(lastDayOfMonth()).atTime(LocalTime.MAX),
            TOP_RESULTS_SIZE
        );
    }
}
