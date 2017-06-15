package com.jvm_bloggers.frontend.admin_area.moderation;

import com.jvm_bloggers.entities.metadata.Metadata;
import com.jvm_bloggers.entities.metadata.MetadataRepository;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.time.LocalDateTime;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER;

public class DateOfLastFetchingDataModel implements IModel<String> {

    private final String name;
    @SpringBean
    private MetadataRepository metadataRepository;

    public DateOfLastFetchingDataModel(String name) {
        this.name = name;
        Injector.get().inject(this);
    }

    @Override
    public String getObject() {
        final Metadata dateOfLastFetch = metadataRepository
                .findByName(name);

        return LocalDateTime.parse(dateOfLastFetch.getValue())
                .format(DATE_TIME_FORMATTER);
    }
}
