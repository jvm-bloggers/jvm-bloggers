package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;


import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.mailing.BlogSummaryMailGenerator;
import pl.tomaszdziurko.jvm_bloggers.mailing.IssueNumberRetriever;
import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender;
import pl.tomaszdziurko.jvm_bloggers.metadata.Metadata;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

@Component
@NoArgsConstructor
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class MailingPageRequestHandler {

    private MailSender mailSender;
    private BlogSummaryMailGenerator blogSummaryMailGenerator;
    private NowProvider nowProvider;
    private MetadataRepository metadataRepository;
    private IssueNumberRetriever issueNumberRetriever;

    public String sendTestEmail() {
        int daysSinceLastFriday = DateTimeUtilities.daysBetweenDateAndLastFriday(nowProvider.now());
        String mailContent = blogSummaryMailGenerator.prepareMailContent(
            daysSinceLastFriday, issueNumberRetriever.getCurrentIssueNumber() + 1
        );
        Metadata testMailAddress = metadataRepository
                .findByName(MetadataKeys.ADMIN_EMAIL);
        mailSender.sendEmail(testMailAddress.getValue(), "[JVM Bloggers] Test mail", mailContent);
        return testMailAddress.getValue();
    }

    public String loadDefaultMailingTemplate() {
        return metadataRepository.findByName(MetadataKeys.DEFAULT_MAILING_TEMPLATE).getValue();
    }
}
