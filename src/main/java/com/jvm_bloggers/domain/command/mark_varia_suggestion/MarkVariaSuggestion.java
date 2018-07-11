package com.jvm_bloggers.domain.command.mark_varia_suggestion;

import com.jvm_bloggers.domain.command.Command;

import lombok.Value;

@Value
public class MarkVariaSuggestion implements Command {
    private final Long id;
}
