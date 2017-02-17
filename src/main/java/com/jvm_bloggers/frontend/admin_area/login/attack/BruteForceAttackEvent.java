package com.jvm_bloggers.frontend.admin_area.login.attack;

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
