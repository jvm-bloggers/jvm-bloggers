package com.jvm_bloggers.domain.command.new_varia_suggestion;

import com.jvm_bloggers.domain.command.Command;

import lombok.AllArgsConstructor;
import lombok.Value;

@Value
@AllArgsConstructor
public class CreateNewVariaSuggestion implements Command {

    private final String url;
    private final String reason;
    private final String author;

}
