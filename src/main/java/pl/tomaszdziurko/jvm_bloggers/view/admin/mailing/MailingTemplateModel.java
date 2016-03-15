package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import org.apache.wicket.model.LoadableDetachableModel;
import pl.tomaszdziurko.jvm_bloggers.metadata.Metadata;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;

class MailingTemplateModel extends LoadableDetachableModel<Metadata> {

    private MetadataRepository metadataRepository;

    public MailingTemplateModel(MetadataRepository metadataRepository) {
        this.metadataRepository = metadataRepository;
    }

    @Override
    protected Metadata load() {
        return metadataRepository.findByName(MetadataKeys.MAILING_TEMPLATE.toString());
    }
}
