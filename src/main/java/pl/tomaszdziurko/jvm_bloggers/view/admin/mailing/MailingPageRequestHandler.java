package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.mailing.BlogSummaryMailGenerator;
import pl.tomaszdziurko.jvm_bloggers.mailing.IssueNumberRetriever;
import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender;
import pl.tomaszdziurko.jvm_bloggers.settings.Setting;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingKeys;
import pl.tomaszdziurko.jvm_bloggers.settings.SettingRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

@Component
public class MailingPageRequestHandler {

    private MailSender mailSender;
    private BlogSummaryMailGenerator blogSummaryMailGenerator;
    private NowProvider nowProvider;
    private SettingRepository settingRepository;
    private IssueNumberRetriever issueNumberRetriever;

    public MailingPageRequestHandler() {
    }

    @Autowired
    public MailingPageRequestHandler(BlogSummaryMailGenerator blogSummaryMailGenerator,
                                     MailSender mailSender,
                                     SettingRepository settingRepository,
                                     IssueNumberRetriever issueNumberRetriever,
                                     NowProvider nowProvider) {
        this.blogSummaryMailGenerator = blogSummaryMailGenerator;
        this.mailSender = mailSender;
        this.settingRepository = settingRepository;
        this.issueNumberRetriever = issueNumberRetriever;
        
        this.nowProvider = nowProvider;
    }

    public String sendTestEmail() {
        int daysSinceLastFriday = DateTimeUtilities.daysBetweenDateAndLastFriday(nowProvider.now());
        String mailContent = blogSummaryMailGenerator.prepareMailContent(
            daysSinceLastFriday, issueNumberRetriever.getCurrentIssueNumber() + 1
        );
        Setting testMailAddress = settingRepository.findByName(SettingKeys.ADMIN_EMAIL.toString());
        mailSender.sendEmail(testMailAddress.getValue(), "[JVM Bloggers] Test mail", mailContent);
        return testMailAddress.getValue();
    }

    public String loadDefaultMailingTemplate() {
        return settingRepository.findByName(SettingKeys.DEFAULT_MAILING_TEMPLATE.toString()).getValue();
    }
}
