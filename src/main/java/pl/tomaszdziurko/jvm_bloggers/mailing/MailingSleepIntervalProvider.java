package pl.tomaszdziurko.jvm_bloggers.mailing;


import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.PRODUCTION;
import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.STAGE;

@Component
@Slf4j
public class MailingSleepIntervalProvider {

    public static final int BASE_DELAY_IN_SECONDS = 120;

    private final List<String> activeProfiles;

    @Autowired
    public MailingSleepIntervalProvider(Environment environment) {
        activeProfiles = Arrays.asList(environment.getActiveProfiles());
    }

    public SleepInterval getSleepingInterval() {
        if (profileIsProductionLike(activeProfiles)) {
            return generateRandomSleepingIntervalForProduction();
        } else {
            return createMailingSleepIntervalForTestsAndDevelopment();
        }
    }

    private boolean profileIsProductionLike(List<String> activeProfiles) {
        return activeProfiles.stream().anyMatch(profile ->
            PRODUCTION.equals(profile) || STAGE.equals(profile));
    }

    @SneakyThrows
    private SleepInterval generateRandomSleepingIntervalForProduction() {
        Random random = new Random();
        int randomDelay = random.nextInt(120);
        int sleepTimeInSeconds = BASE_DELAY_IN_SECONDS + randomDelay;
        SleepInterval sleepInterval = new SleepInterval(TimeUnit.SECONDS, sleepTimeInSeconds);
        log.debug("Sleep interval: {}", sleepInterval);
        return sleepInterval;
    }

    private SleepInterval createMailingSleepIntervalForTestsAndDevelopment() {
        return SleepInterval.NO_SLEEPING;
    }
}
