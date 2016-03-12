package pl.tomaszdziurko.jvm_bloggers.view.login.attack;

import lombok.Builder;
import lombok.Data;

/**
 * @author Adam Dec
 * @since 0.7.0
 */
@Data
@Builder
public class BruteForceAttackEvent {

   private final String ipAddress;
}