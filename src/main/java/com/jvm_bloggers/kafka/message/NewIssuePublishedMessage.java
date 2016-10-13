package com.jvm_bloggers.kafka.message;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Value;

@KafkaMessage
@Value
public class NewIssuePublishedMessage {
    @JsonProperty("issueNumber")
    private Long issueNumber;
    @JsonProperty("url")
    private String url;
}
