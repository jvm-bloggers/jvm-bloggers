package com.jvm_bloggers.utils

import com.jvm_bloggers.SpringContextAwareSpecification
import org.jasypt.encryption.StringEncryptor
import org.springframework.beans.factory.annotation.Autowired

/**
 * This test is a helper to encrypt properties we don't want to put in the repo as a plain text.
 *
 * Simply replace PASSWORD and textToEncrypt with your values and run this spec, then put generated values in properties file.
 *
 */
class JasyptShouldEncryptPropertiesSpec extends SpringContextAwareSpecification {

    static String PASSWORD = "secretPassword"

    @Override
    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", PASSWORD)
    }

    @Autowired
    StringEncryptor encryptor

    def "Should encrypt and decrypt given value"() {
        given:
        String textToEncrypt = "textToEncrypt"

        when:
        String encrypted = encryptor.encrypt(textToEncrypt)
        String decryptedText = encryptor.decrypt(encrypted)
        println """
                * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                Encrypting...
                    original text = $textToEncrypt
                    encrypted text = $encrypted
                * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                """
        then:
        textToEncrypt == decryptedText
    }
}