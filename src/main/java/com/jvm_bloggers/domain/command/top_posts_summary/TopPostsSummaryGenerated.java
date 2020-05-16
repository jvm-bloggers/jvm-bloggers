package com.jvm_bloggers.domain.command.top_posts_summary;

import com.jvm_bloggers.core.utils.JvmBloggersEvent;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.time.YearMonth;

@JvmBloggersEvent
@RequiredArgsConstructor
@Getter
public class TopPostsSummaryGenerated {
    private final YearMonth yearMonth;
}
