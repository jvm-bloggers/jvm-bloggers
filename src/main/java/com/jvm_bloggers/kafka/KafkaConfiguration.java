package com.jvm_bloggers.kafka;

import lombok.Getter;
import lombok.ToString;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@ToString
@Getter
@Component
public class KafkaConfiguration {

    private final String adress;
    private final int streamBufferSize;
    private final String newIssueTopic;

    @Autowired
    public KafkaConfiguration(
            @Value("${kafka.topics.new.issue}") String newIssueTopic,
            @Value("${kafka.adress}") String adress,
            @Value("${kafka.stream.buffer.size}") int streamBufferSize) {
        this.newIssueTopic = newIssueTopic;
        this.adress = adress;
        this.streamBufferSize = streamBufferSize;
    }
}
