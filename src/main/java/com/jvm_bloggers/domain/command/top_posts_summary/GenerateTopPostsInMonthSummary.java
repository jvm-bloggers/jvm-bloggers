package com.jvm_bloggers.domain.command.top_posts_summary;

import com.jvm_bloggers.domain.command.Command;
import lombok.RequiredArgsConstructor;
import lombok.Value;

import java.time.YearMonth;

@Value
@RequiredArgsConstructor
public class GenerateTopPostsInMonthSummary implements Command {

    private final YearMonth yearMonth;
    private final int numberOfPersonalPosts;
    private final int numberOfCompanyPosts;

}
