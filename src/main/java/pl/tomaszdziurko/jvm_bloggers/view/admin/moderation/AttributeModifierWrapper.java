package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.behavior.AttributeAppender;
import org.springframework.stereotype.Component;

import java.io.Serializable;

/**
 * @autor mszarlinski
 */
@Component
public class AttributeModifierWrapper {

    public AttributeAppender append(String attributeName, Serializable value) {
        return AttributeModifier.append(attributeName, value);
    }
}
