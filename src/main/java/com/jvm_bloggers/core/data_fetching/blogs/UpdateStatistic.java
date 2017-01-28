package com.jvm_bloggers.core.data_fetching.blogs;

import lombok.Getter;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;

@Getter
public class UpdateStatistic {

    private final int updated;
    private final int created;
    private final int invalid;
    private final int notChanged;
    private final int total;

    private UpdateStatistic(Map<UpdateStatus, Integer> stats) {
        updated = stats.getOrDefault(UpdateStatus.UPDATED, 0);
        created = stats.getOrDefault(UpdateStatus.CREATED, 0);
        invalid = stats.getOrDefault(UpdateStatus.INVALID, 0);
        notChanged = stats.getOrDefault(UpdateStatus.NOT_CHANGED, 0);
        total = updated + created + invalid + notChanged;
    }

    public static Collector<UpdateStatus, ?, UpdateStatistic> collector() {
        return Collectors.collectingAndThen(
            Collectors.groupingByConcurrent(
                Function.identity(),
                Collectors.reducing(0, e -> 1, Integer::sum)),
            UpdateStatistic::new);
    }

    @Override
    public String toString() {
        return String.format("totalRecords=%d, updated=%d, new=%d, invalid=%d, notChanged=%d",
            total, updated, created, invalid, notChanged);
    }
}
