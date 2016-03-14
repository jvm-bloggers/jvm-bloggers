package pl.tomaszdziurko.jvm_bloggers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class SpringApplicationContext {

    private static ApplicationContext context;

    @Autowired
    public SpringApplicationContext(ApplicationContext context) {
        SpringApplicationContext.context = context;
    }

    public static <T> T getBean(Class<T> clazz) {
        return context.getBean(clazz);
    }
}