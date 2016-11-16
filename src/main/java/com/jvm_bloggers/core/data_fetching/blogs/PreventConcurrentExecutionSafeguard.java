package com.jvm_bloggers.core.data_fetching.blogs;

import com.google.common.base.Supplier;

import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.atomic.AtomicReference;

@Slf4j
public class PreventConcurrentExecutionSafeguard<T> {

    private final AtomicReference<Boolean> fetchingInProgress = new AtomicReference<>(false);

    public T prevenConcurrentExecution(Supplier<T> task) {
        T result = null;
        try {
            if (fetchingInProgress.compareAndSet(false, true)) {
                result = task.get();
            } else {
                log.info("The {} is being executed by another thread. Skipping...", task);
            }
        } finally {
            fetchingInProgress.set(false);
        }
        return result;
    }

    public boolean isExecuting() {
        return fetchingInProgress.get();
    }

}
