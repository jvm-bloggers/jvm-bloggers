package com.jvm_bloggers.kafka.serialization;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jvm_bloggers.kafka.exception.UnableToSerializeMessageException;

import org.springframework.stereotype.Component;

@Component
public class MessageSerializer {
    private final ObjectMapper objectMapper;

    public MessageSerializer() {
        objectMapper = new ObjectMapper();
    }

    public <T> String serialize(T message) {
        try {
            return objectMapper.writeValueAsString(message);
        } catch (JsonProcessingException jsonException) {
            throw new UnableToSerializeMessageException(jsonException,message);
        }
    }

}
