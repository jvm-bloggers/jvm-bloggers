package pl.tomaszdziurko.jvm_bloggers.jasypt

import org.jasypt.encryption.StringEncryptor
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ContextConfiguration
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import spock.lang.Specification

/**
 * This test is a helper to encrypt properties we don't want to put in the repo as a plain text.
 *
 * Simply replace PASSWORD and textToEncrypt with your values and run this spec, then put generated values in properties file.
 *
 */

@ContextConfiguration(classes = [JvmBloggersApplication], loader = SpringApplicationContextLoader)
class JasyptShouldEncryptPropertiesSpec extends Specification {

    static String PASSWORD = "secretPassword";

    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", PASSWORD);
    }

    @Autowired
    StringEncryptor encryptor;

    def "Should encrypt and decrypt given value"() {
        given:
            String textToEncrypt = "textToEncrypt"
        when:
            String encrypted = encryptor.encrypt(textToEncrypt);
            String decryptedText = encryptor.decrypt(encrypted);
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