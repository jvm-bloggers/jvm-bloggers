package com.jvm_bloggers.core.data_fetching.blogs;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicBoolean;

@Slf4j
public class PreventConcurrentExecutionSafeguard {

    private final AtomicBoolean isExecuting = new AtomicBoolean();

    @SneakyThrows
    public void preventConcurrentExecution(Callable<Void> task) {
        try {
            if (isExecuting.compareAndSet(false, true)) {
                log.info("About to execute task {} in thread {}", task, Thread.currentThread());
                task.call();
            } else {
                log.info("The {} is being executed by another thread. Skipping...", task);
            }
        } finally {
            log.info("Finished executing/skipping {} in thread {}", task, Thread.currentThread());
            isExecuting.set(false);
        }
    }

    public boolean isExecuting() {
        return isExecuting.get();
    }

}
