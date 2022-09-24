package com.jvm_bloggers.validation

import com.networknt.schema.ValidationMessage

import static com.networknt.schema.ValidatorTypeCode.*

class ValidatorMessageTranslator {

    private ValidatorMessageTranslator() {
        throw new UnsupportedOperationException()
    }

    static String translate(final Set<ValidationMessage> messages) {
        return messages
                .collect { createMessage(it) }
                .join("\n")
    }

    private static String createMessage(final ValidationMessage message) {
        switch (message.type) {
            case REQUIRED.value:
                return "Element '$message.path' does not contain required elements '$message.arguments'!"
            case PATTERN.value:
                return "Element '$message.path' does not match the required pattern '$message.arguments'!"
            case MIN_LENGTH.value:
            case MAX_LENGTH.value:
                return "Element '$message.path' has an invalid '$message.type' length - the valid one is '$message.arguments'!"
            case UNIQUE_ITEMS.value:
                return "Array '$message.path' contains not unique elements!"
            default:
                return defaultViolation(message)
        }
    }

    private static String defaultViolation(final ValidationMessage message) {
        Optional.of(message)
                .map { it.message }
                .filter { !it.isBlank() }
                .filter { it.contains(":") }
                .map { it.substring(it.indexOf(':') + 2) }
                .map { "Element '$message.path' violates '$message.type' rule - '$it'" }
                .map { it.toString() }
                .orElse(message.message)
    }
}
