package com.jvm_bloggers.domain.command.save_metadata;

import com.jvm_bloggers.domain.command.CommandHandler;
import com.jvm_bloggers.entities.metadata.MetadataRepository;

import lombok.RequiredArgsConstructor;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class SaveMetadataCommandHandler implements CommandHandler<SaveMetadata> {

    private final MetadataRepository metadataRepository;

    @Override
    @EventListener
    public void handle(SaveMetadata command) {
        metadataRepository.save(command.getMetadata());
    }
}
