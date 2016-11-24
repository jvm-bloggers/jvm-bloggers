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
                log.debug("About to execute task {} in thread {}", task, Thread.currentThread());
                task.call();
            } else {
                log.info("The {} is being executed by another thread. Skipping...", task);
            }
        } finally {
            isExecuting.set(false);
        }
    }

    public boolean isExecuting() {
        return isExecuting.get();
    }

}
