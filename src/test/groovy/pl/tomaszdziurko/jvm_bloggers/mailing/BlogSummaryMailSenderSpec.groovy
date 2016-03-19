package pl.tomaszdziurko.jvm_bloggers.mailing

import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddress
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddressRepository
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Ignore
import spock.lang.Specification
import spock.lang.Subject

class BlogSummaryMailSenderSpec extends Specification {

    BlogSummaryMailGenerator blogSummaryMailGenerator = Stub(BlogSummaryMailGenerator)
    MailSender mailSender = Mock(MailSender)
    MailingAddressRepository mailingAddressRepository = Stub(MailingAddressRepository)
    IssueNumberRetriever issueNumberRetriever = Stub(IssueNumberRetriever)
    MailingSleepIntervalProvider mailingSleepIntervalProvider = Stub(MailingSleepIntervalProvider)

    @Subject
    BlogSummaryMailSender summaryMailSender = new BlogSummaryMailSender(
            blogSummaryMailGenerator,
            mailSender,
            mailingAddressRepository,
            issueNumberRetriever,
            mailingSleepIntervalProvider,
            new NowProvider())

    def setup() {
        blogSummaryMailGenerator.prepareMailContent(_) >> "Some generated mail content"
        mailingSleepIntervalProvider.getSleepingInterval() >> SleepInterval.NO_SLEEPING
    }

    def "Should not send any mail for empty MailingAddress DB table"() {
        given:
            mailingAddressRepository.findAll() >>  []
        when:
            summaryMailSender.sendSummary(10)
        then:
            0 * mailSender.sendEmail(_, _, _)
    }

    @Ignore("until #157 is fixed")
    def "Should send two emails for two records in MailingAddress"() {
        given:
            mailingAddressRepository.findAll() >>  [new MailingAddress("email@email.com"), new MailingAddress("email2@email2.com")]
        when:
            summaryMailSender.sendSummary(10)
        then:
            2 * mailSender.sendEmail(_, _, _)
    }

}
