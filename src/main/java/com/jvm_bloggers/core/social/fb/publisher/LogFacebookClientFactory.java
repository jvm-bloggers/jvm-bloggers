package com.jvm_bloggers.core.social.fb.publisher;

import com.restfb.BinaryAttachment;
import com.restfb.DefaultFacebookClient;
import com.restfb.FacebookClient;
import com.restfb.Parameter;
import com.restfb.Version;

import lombok.extern.slf4j.Slf4j;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.List;

import static com.jvm_bloggers.ApplicationProfiles.DEV;
import static com.jvm_bloggers.ApplicationProfiles.STAGE;
import static com.jvm_bloggers.ApplicationProfiles.TEST;

@Slf4j
@Component
@Profile({DEV, STAGE, TEST})
class LogFacebookClientFactory implements FacebookClientFactory {

    @Override
    public FacebookClient createFacebookClient() {
        return new DefaultFacebookClient(Version.LATEST) {
            @Override
            public <T> T publish(String connection,
                                 Class<T> objectType,
                                 Parameter... parameters) {
                log.info("Publishing {}", (Object[]) parameters);
                return jsonMapper.toJavaObject("published", objectType);
            }

            @Override
            public <T> T publish(String connection,
                                 Class<T> objectType,
                                 List<BinaryAttachment> binaryAttachments,
                                 Parameter... parameters) {
                log.info("Publishing {}", (Object[]) parameters);
                return jsonMapper.toJavaObject("published", objectType);
            }

            @Override
            public <T> T publish(String connection,
                                 Class<T> objectType,
                                 BinaryAttachment binaryAttachment,
                                 Parameter... parameters) {
                log.info("Publishing {}", (Object[]) parameters);
                return jsonMapper.toJavaObject("published", objectType);
            }
        };
    }

}
