package com.jvm_bloggers.admin_panel.login.attack;

import com.google.common.cache.CacheBuilder;

import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
public class BruteForceLoginAttackDetector {

    private static final int MAX_INVALID_LOGIN_ATTEMPTS_PER_IP_ADDRESS = 3;
    private static final int EXPIRY_TIME_IN_MINUTES = 10;

    private ConcurrentMap<String, Integer> invalidLoginAttemptsCounter;

    public BruteForceLoginAttackDetector() {
        this(EXPIRY_TIME_IN_MINUTES, TimeUnit.MINUTES);
    }

    public BruteForceLoginAttackDetector(long duration, TimeUnit timeUnit) {
        invalidLoginAttemptsCounter = CacheBuilder
            .newBuilder()
            .expireAfterWrite(duration, timeUnit)
            .<String, Integer>build()
            .asMap();
    }

    public void recordInvalidLoginAttempt(String clientAddress) {
        log.info("Storing invalid login attempt from {}", clientAddress);
        invalidLoginAttemptsCounter.merge(clientAddress, 1, Integer::sum);
    }

    public boolean isItBruteForceAttack(String clientAddress) {
        Integer invalidLoginCounter = invalidLoginAttemptsCounter.getOrDefault(clientAddress, -1);
        boolean bruteForceAttackDetected =
            invalidLoginCounter >= MAX_INVALID_LOGIN_ATTEMPTS_PER_IP_ADDRESS;
        if (bruteForceAttackDetected) {
            log.warn("Brute force attack detected from {}", clientAddress);
        }
        return bruteForceAttackDetected;
    }
}
