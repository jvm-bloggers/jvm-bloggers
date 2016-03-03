package pl.tomaszdziurko.jvm_bloggers

import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ActiveProfiles
import org.springframework.test.context.ContextConfiguration

import spock.lang.Specification;

@ContextConfiguration(classes = [JvmBloggersApplication], loader = SpringApplicationContextLoader)
@ActiveProfiles("test")
public abstract class SpringContextAwareSpecification extends Specification {

    static String PASSWORD = "secretPassword";
    
    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", PASSWORD);
    }

}
