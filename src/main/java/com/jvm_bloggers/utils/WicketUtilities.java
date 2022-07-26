package com.jvm_bloggers.utils;

import lombok.experimental.UtilityClass;
import org.apache.wicket.markup.head.MetaDataHeaderItem;

@UtilityClass
public class WicketUtilities {
    public static MetaDataHeaderItem forMetaTag(String name, String value) {
        final var metaDataHeaderItem = MetaDataHeaderItem.forMetaTag(name, value);
        metaDataHeaderItem.addTagAttribute("property", name);

        return metaDataHeaderItem;
    }
}
