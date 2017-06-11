package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.domain.command.CommandPublisher;
import com.jvm_bloggers.domain.command.top_posts_summary.GenerateTopPostsInMonthSummary;
import com.jvm_bloggers.utils.NowProvider;
import org.springframework.stereotype.Service;

import java.time.YearMonth;

@Service
public class AdminDashboardPageBackingBean {

    private final NowProvider nowProvider;
    private final CommandPublisher commandPublisher;

    public AdminDashboardPageBackingBean(
        NowProvider nowProvider,
        CommandPublisher commandPublisher
    ) {
        this.nowProvider = nowProvider;
        this.commandPublisher = commandPublisher;
    }

    public void generateTopPostsSummary() {
        YearMonth previousMonth = YearMonth.from(nowProvider.today().minusMonths(1));
        commandPublisher.publish(
            new GenerateTopPostsInMonthSummary(previousMonth, 10, 5)
        );
    }

}
