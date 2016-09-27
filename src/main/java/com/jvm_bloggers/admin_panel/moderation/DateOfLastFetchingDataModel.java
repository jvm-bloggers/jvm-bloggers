package com.jvm_bloggers.admin_panel.moderation;

import com.jvm_bloggers.core.metadata.Metadata;
import com.jvm_bloggers.core.metadata.MetadataRepository;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class DateOfLastFetchingDataModel extends AbstractReadOnlyModel<String> {

    @SpringBean
    private MetadataRepository metadataRepository;

    private final String name;

    public DateOfLastFetchingDataModel(String name) {
        this.name = name;
        Injector.get().inject(this);
    }

    @Override
    public String getObject() {
        final Metadata dateOfLastFetch = metadataRepository
            .findByName(name);

        return LocalDateTime.parse(dateOfLastFetch.getValue())
            .format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:SS"));
    }
}
