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
abstract class SpringContextAwareSpecification extends Specification {

    static String PASSWORD = "secretPassword"

    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", PASSWORD)
    }

}
