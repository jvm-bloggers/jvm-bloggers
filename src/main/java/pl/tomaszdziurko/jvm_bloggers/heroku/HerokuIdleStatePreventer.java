package pl.tomaszdziurko.jvm_bloggers.heroku;


import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import pl.tomaszdziurko.jvm_bloggers.TimeConstants;

@Component
@Slf4j
public class HerokuIdleStatePreventer {

    private static final String APPLICATION_URL = "http://jvm-bloggers.herokuapp.com/info";

    @Scheduled(cron = TimeConstants.EVERY_55TH_MINUTE_OF_EACH_HOUR_BETWEEN_8_AND_23)
    public void pingHerokuApp() {
        log.info("Pinging " + APPLICATION_URL);
        new RestTemplate().getForObject(APPLICATION_URL, Object.class);
    }
}
