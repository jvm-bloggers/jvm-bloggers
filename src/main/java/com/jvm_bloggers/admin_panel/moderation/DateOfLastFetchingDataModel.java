package com.jvm_bloggers.admin_panel.moderation;

import com.jvm_bloggers.core.metadata.Metadata;
import com.jvm_bloggers.core.metadata.MetadataRepository;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.time.LocalDateTime;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER;

public class DateOfLastFetchingDataModel extends AbstractReadOnlyModel<String> {

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
