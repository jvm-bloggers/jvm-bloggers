package com.jvm_bloggers.frontend.public_area.varia_suggestion;

import lombok.Data;

import java.io.Serializable;

@Data
public class VariaSuggestionModel implements Serializable {
    private String url;
    private String reason;
    private String author;
}
