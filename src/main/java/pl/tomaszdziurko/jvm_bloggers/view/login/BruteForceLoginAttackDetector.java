package pl.tomaszdziurko.jvm_bloggers.view.login;


import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

@Component
@Slf4j
public class BruteForceLoginAttackDetector {

    private static final int MAX_INVALID_LOGIN_ATTEMPTS_PER_IP_ADDRESS = 3;
    private static final int EXPIRY_TIME_IN_MINUTES = 10;

    private Cache<String, Integer> invalidLoginAttemptsCounter;

    public BruteForceLoginAttackDetector() {
        this(EXPIRY_TIME_IN_MINUTES, TimeUnit.MINUTES);
    }

    public BruteForceLoginAttackDetector(long duration, TimeUnit timeUnit) {
        invalidLoginAttemptsCounter = CacheBuilder
            .newBuilder()
            .expireAfterWrite(duration, timeUnit).build();
    }

    public synchronized void recordInvalidLoginAttempt(String clientAddress) {
        log.info("Storing invalid login attempt from " + clientAddress);
        Integer invalidLoginCounter = invalidLoginAttemptsCounter.getIfPresent(clientAddress);
        if (invalidLoginCounter == null) {
            invalidLoginAttemptsCounter.put(clientAddress, 1);
        } else {
            invalidLoginAttemptsCounter.put(clientAddress, invalidLoginCounter + 1);
        }
    }

    public boolean isItBruteForceAttack(String clientAddress) {
        Integer invalidLoginCounter = invalidLoginAttemptsCounter.getIfPresent(clientAddress);
        boolean bruteForceAttackDetected = invalidLoginCounter != null && invalidLoginCounter >= MAX_INVALID_LOGIN_ATTEMPTS_PER_IP_ADDRESS;
        if (bruteForceAttackDetected) {
            log.warn("Brute force attack detected from " + clientAddress);
        }
        return bruteForceAttackDetected;
    }

}
