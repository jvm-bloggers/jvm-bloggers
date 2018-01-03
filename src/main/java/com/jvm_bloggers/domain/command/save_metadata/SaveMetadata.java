package com.jvm_bloggers.domain.command.save_metadata;

import com.jvm_bloggers.domain.command.Command;
import com.jvm_bloggers.entities.metadata.Metadata;

import lombok.RequiredArgsConstructor;
import lombok.Value;

@Value
@RequiredArgsConstructor
public class SaveMetadata implements Command {

    private final Metadata metadata;

}
