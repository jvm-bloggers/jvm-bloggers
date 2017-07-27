package com.jvm_bloggers.frontend

class WicketTestUtils {

    private static String SEPARATOR = ":"

    static String pathVia(Object... componentIds) {
        return componentIds.toList().join(SEPARATOR)
    }

}
