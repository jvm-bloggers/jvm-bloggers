package com.jvm_bloggers.domain.query.unread_varia_suggestion;

import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestion;

import lombok.Value;

import java.io.Serializable;
import java.time.LocalDateTime;

@Value
public class UnreadVariaSuggestion implements Serializable {

    private final Long id;
    private final String url;
    private final String author;
    private final String reason;
    private final LocalDateTime createDate;

    public static UnreadVariaSuggestion from(VariaSuggestion variaSuggestion) {
        return new UnreadVariaSuggestion(
            variaSuggestion.getId(),
            variaSuggestion.getUrl(),
            variaSuggestion.getAuthor(),
            variaSuggestion.getReason(),
            variaSuggestion.getCreateDate()
        );
    }
}
