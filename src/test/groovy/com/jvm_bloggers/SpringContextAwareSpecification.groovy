package com.jvm_bloggers

import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.ActiveProfiles
import org.springframework.test.context.ContextConfiguration
import spock.lang.Specification

import javax.transaction.Transactional

@ContextConfiguration
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public abstract class SpringContextAwareSpecification extends Specification {

    static String PASSWORD = "secretPassword"
    static String FB_USER_TOKEN = "fbUserToken"
    static String FB_PAGE_ID = "fbPageId"
    static String FB_APP_SECRET = "fbAppSecret"

    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", PASSWORD)
        System.setProperty("fb.user.token", FB_USER_TOKEN)
        System.setProperty("fb.page.id", FB_PAGE_ID)
        System.setProperty("fb.app.secret", FB_APP_SECRET)
    }

}
