package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import org.apache.wicket.model.LoadableDetachableModel;
import pl.tomaszdziurko.jvm_bloggers.settings.Setting;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingKeys;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingRepository;

class MailingTemplateModel extends LoadableDetachableModel<Setting> {

    private SettingRepository settingRepository;

    public MailingTemplateModel(SettingRepository settingRepository) {
        super();
        this.settingRepository = settingRepository;
    }

    @Override
    protected Setting load() {
        return settingRepository.findByName(SettingKeys.MAILING_TEMPLATE.toString());
    }
}
