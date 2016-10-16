package com.jvm_bloggers.kafka.exception;

import com.jvm_bloggers.kafka.KafkaConfiguration;

public class UnableToConnectToKafkaException extends RuntimeException {
    public UnableToConnectToKafkaException(Throwable cause, KafkaConfiguration configuration) {
        super("Unable to connect to kafka. Config: " + configuration,cause);
    }
}
