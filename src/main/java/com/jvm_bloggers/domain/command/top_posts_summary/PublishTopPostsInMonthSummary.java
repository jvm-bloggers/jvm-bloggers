package com.jvm_bloggers.domain.command.top_posts_summary;

import com.jvm_bloggers.domain.command.Command;
import com.jvm_bloggers.entities.click.PostIdWithCount;
import javaslang.collection.List;
import lombok.RequiredArgsConstructor;
import lombok.Value;

import java.time.YearMonth;

@Value
@RequiredArgsConstructor
public class PublishTopPostsInMonthSummary implements Command {

    private final List<PostIdWithCount> bestPersonalPosts;
    private final List<PostIdWithCount> bestCompanyPosts;
    private final YearMonth yearMonth;

}
