package com.jvm_bloggers.domain.query;

import com.jvm_bloggers.entities.metadata.Metadata;
import com.jvm_bloggers.entities.metadata.MetadataRepository;

import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class MetadataQuery {

    private final MetadataRepository metadataRepository;

    public Metadata findByName(String name) {
        return metadataRepository.findByName(name);
    }
}
