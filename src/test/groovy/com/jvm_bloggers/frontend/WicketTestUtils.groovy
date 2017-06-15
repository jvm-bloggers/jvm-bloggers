package com.jvm_bloggers.frontend

class WicketTestUtils {

    private static String SEPARATOR = ":"

    static String pathVia(String... componentIds) {
        return componentIds.toList().join(SEPARATOR)
    }

}
