package com.jvm_bloggers.frontend.admin_area.login.attack;

import com.jvm_bloggers.utils.NowProvider;

import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Component;
import org.stringtemplate.v4.ST;

import java.time.format.DateTimeFormatter;

/**
 * @author Adam Dec
 */
@Component
@RequiredArgsConstructor
public class BruteForceAttackMailGenerator {

    private static final String TIME = "Time";
    private static final String IP_ADDRESS = "IPAddress";
    private static final String DEFAULT_EMAIL_TITLE =
        "Brute force attack detected for <" + IP_ADDRESS + ">";
    private static final String ATTACK_BRUTE_FORCE_MAIL_TEMPLATE =
        "Brute Force attack detected from IP: <" + IP_ADDRESS + "> at <" + TIME + ">";
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss");

    private final NowProvider nowProvider;

    public String prepareMailContent(@NonNull BruteForceAttackEvent bruteForceAttackEvent) {
        final ST template = new ST(ATTACK_BRUTE_FORCE_MAIL_TEMPLATE);
        template.add(IP_ADDRESS, bruteForceAttackEvent.getIpAddress());
        template.add(TIME, nowProvider.now().format(FORMATTER));
        return template.render();
    }

    public String prepareMailTitle(@NonNull BruteForceAttackEvent bruteForceAttackEvent) {
        final ST template = new ST(DEFAULT_EMAIL_TITLE);
        template.add(IP_ADDRESS, bruteForceAttackEvent.getIpAddress());
        return template.render();
    }
}
