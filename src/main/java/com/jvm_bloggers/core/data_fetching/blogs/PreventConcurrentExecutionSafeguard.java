package com.jvm_bloggers.core.data_fetching.blogs;

import com.google.common.base.Supplier;

import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.atomic.AtomicBoolean;

@Slf4j
public class PreventConcurrentExecutionSafeguard<T> {

    private final AtomicBoolean isExecuting = new AtomicBoolean();

    public T preventConcurrentExecution(Supplier<T> task) {
        T result = null;
        try {
            if (isExecuting.compareAndSet(false, true)) {
                log.debug("About to execute task {} in thread {}", task, Thread.currentThread());
                result = task.get();
            } else {
                log.info("The {} is being executed by another thread. Skipping...", task);
            }
        } finally {
            isExecuting.set(false);
        }
        return result;
    }

    public boolean isExecuting() {
        return isExecuting.get();
    }

}
