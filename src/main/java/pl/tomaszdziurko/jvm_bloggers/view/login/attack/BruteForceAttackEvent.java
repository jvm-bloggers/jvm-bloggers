package pl.tomaszdziurko.jvm_bloggers.view.login.attack;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class BruteForceAttackEvent {

   private final String clientAddress;
}
