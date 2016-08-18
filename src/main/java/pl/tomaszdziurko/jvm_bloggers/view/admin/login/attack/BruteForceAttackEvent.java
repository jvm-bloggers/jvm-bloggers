package pl.tomaszdziurko.jvm_bloggers.view.admin.login.attack;

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
