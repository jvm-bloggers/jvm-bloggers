package pl.tomaszdziurko.jvm_bloggers;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = JvmBloggersApplication.class)
@WebAppConfiguration
@ActiveProfiles("test")
public class JvmBloggersApplicationTests {

    @BeforeClass
    public static void setup() {
        System.setProperty("jasypt.encryptor.password", "password");
    }

    @Test
    public void contextLoads() {

    }

}
