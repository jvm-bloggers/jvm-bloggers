package pl.tomaszdziurko.jvm_bloggers.mailing

import spock.lang.Specification
import spock.lang.Unroll

class NewsletterRecipientsProviderSpec extends Specification {

    @Unroll
    def "Should correctly parse comma separated mail: #commaSeparatedEmails"() {
        when:
            NewsletterRecipientsProvider provider = new NewsletterRecipientsProvider(commaSeparatedEmails);
        then:
            provider.getRecipients() == expectedEmailsList
        where:
            commaSeparatedEmails                     | expectedEmailsList
            "jan.john"                               | ["jan.john@gmail.com"]
            "  jan.john   "                               | ["jan.john@gmail.com"]
            "jan.kowalski@ex.com"               | ["jan.kowalski@ex.com"]
            "jan.kowalski@ex.com,piotr.nowak@eq.com" | ["jan.kowalski@ex.com", "piotr.nowak@eq.com"]
            "jan.john, piotr.nowak@ggg.com"          | ["jan.john@gmail.com", "piotr.nowak@ggg.com"]

    }


}
