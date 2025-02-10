package com.jvm_bloggers.entities.click;

import lombok.Value;

import java.io.Serializable;

@Value
public class PostClicksCountByIpAddress implements Serializable {

    private final String title;
    private final String author;
    private final String ipAddress;
    private final long counter;

}
