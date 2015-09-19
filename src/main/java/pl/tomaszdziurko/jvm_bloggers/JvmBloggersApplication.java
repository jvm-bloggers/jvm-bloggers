package pl.tomaszdziurko.jvm_bloggers;

import com.ulisesbocchio.jasyptspringboot.annotation.EnableEncryptableProperties;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
@EnableEncryptableProperties
public class JvmBloggersApplication {

    public static void main(String[] args) {
        SpringApplication.run(JvmBloggersApplication.class, args);
    }

}
