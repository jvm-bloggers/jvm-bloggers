package com.jvm_bloggers.admin_panel.login.attack;

import lombok.Builder;
import lombok.Data;

/**
 * @author Adam Dec
 */
@Data
@Builder
public class BruteForceAttackEvent {

    private final String ipAddress;
}
