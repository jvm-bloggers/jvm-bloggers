package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import org.apache.wicket.model.LoadableDetachableModel;
import pl.tomaszdziurko.jvm_bloggers.settings.Metadata;
import pl.tomaszdziurko.jvm_bloggers.settings.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.settings.MetadataRepository;

class MailingTemplateModel extends LoadableDetachableModel<Metadata> {

    private MetadataRepository metadataRepository;

    public MailingTemplateModel(MetadataRepository metadataRepository) {
        super();
        this.metadataRepository = metadataRepository;
    }

    @Override
    protected Metadata load() {
        return metadataRepository.findByName(MetadataKeys.MAILING_TEMPLATE.toString());
    }
}
