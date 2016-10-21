package com.jvm_bloggers.kafka;

import lombok.Getter;
import lombok.ToString;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@ToString
@Getter
@Configuration
public class KafkaConfiguration {

    private final String address;
    private final int streamBufferSize;
    private final String newIssueTopic;

    @Autowired
    public KafkaConfiguration(
            @Value("${kafka.topics.new.issue}") String newIssueTopic,
            @Value("${kafka.address}") String address,
            @Value("${kafka.stream.buffer.size}") int streamBufferSize) {
        this.newIssueTopic = newIssueTopic;
        this.address = address;
        this.streamBufferSize = streamBufferSize;
    }
}
