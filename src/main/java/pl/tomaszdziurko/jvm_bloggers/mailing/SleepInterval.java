package pl.tomaszdziurko.jvm_bloggers.mailing;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.ToString;

import java.util.concurrent.TimeUnit;

@AllArgsConstructor
@ToString
@EqualsAndHashCode
public class SleepInterval {

    public static final SleepInterval NO_SLEEPING =
        new SleepInterval(TimeUnit.SECONDS, 0);

    final long milliseconds;

    public SleepInterval(@NonNull TimeUnit timeUnit,
                         long numberOfTimeUnits) {
        milliseconds = timeUnit.toMillis(numberOfTimeUnits);
    }

    long asMilliseconds() {
        return milliseconds;
    }

    long asSeconds() {
        return TimeUnit.MILLISECONDS.toSeconds(milliseconds);
    }

}
