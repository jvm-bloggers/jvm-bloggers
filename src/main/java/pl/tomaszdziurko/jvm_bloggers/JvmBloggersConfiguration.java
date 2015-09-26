package pl.tomaszdziurko.jvm_bloggers;


import akka.actor.ActorSystem;
import com.sendgrid.SendGrid;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class JvmBloggersConfiguration {

    @Bean
    public ActorSystem getActorSystem() {
        ActorSystem system = ActorSystem.create("jvm-bloggers-akka");
        return system;
    }

    @Bean
    public SendGrid getSendGrid(@Value("${sendgrid.apiKey}") String sendGridApiKey) {
        return new SendGrid(sendGridApiKey);
    }

}
