package pl.tomaszdziurko.jvm_bloggers.mailing

import org.springframework.core.env.Environment
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.*

class MailingSleepIntervalProviderSpec extends Specification {

    @Unroll
    def "Should return SleepingInterval #interval ms for active profiles #activeProfiles"() {
        given:
            Environment environment = mockEnvironmentWithProfiles(activeProfiles)
            @Subject MailingSleepIntervalProvider provider =
                    new MailingSleepIntervalProvider(environment)
        when:
            SleepInterval sleepingInterval = provider.getSleepingInterval()
        then:
            sleepingInterval.asMilliseconds() == 0L
        where:
            activeProfiles || _
            [TEST]         || _
            [TEST, DEV]    || _
            [DEV]          || _
    }

    @Unroll
    def "Should return random SleepingInterval greater or equal base delay for #activeProfiles"() {
        given:
            Environment environment = mockEnvironmentWithProfiles(activeProfiles)
            @Subject MailingSleepIntervalProvider provider =
                    new MailingSleepIntervalProvider(environment)
        when:
            SleepInterval sleepingInterval = provider.getSleepingInterval()
        then:
            sleepingInterval.asSeconds() >= MailingSleepIntervalProvider.BASE_DELAY_IN_SECONDS
        where:
            activeProfiles      || _
            [PRODUCTION]        || _
            [STAGE, PRODUCTION] || _
            [STAGE]             || _
    }

    private Environment mockEnvironmentWithProfiles(ArrayList<String> activeProfiles) {
        Environment environment = Mock(Environment) {
            getActiveProfiles() >> activeProfiles
        }
        return environment
    }
}
