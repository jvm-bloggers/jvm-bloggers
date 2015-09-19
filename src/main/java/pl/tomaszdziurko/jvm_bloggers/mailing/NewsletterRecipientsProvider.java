package pl.tomaszdziurko.jvm_bloggers.mailing;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@Component
@Getter
public class NewsletterRecipientsProvider {

    public static final String ADDRESS_POSTFIX = "@" + "g" + "mai" + "l.co" + "m"; // die spamming bots, die, die, die! :)

    private final List<String> recipients;

    @Autowired
    public NewsletterRecipientsProvider(@Value("${mails.to.send.weekly.summary}") String commaSeparatedListOfAddresses) {
        recipients = Arrays.stream(commaSeparatedListOfAddresses.split(","))
            .map(rawAddress -> {
                    if (!rawAddress.contains("@")) {
                        return rawAddress.trim() + ADDRESS_POSTFIX;
                    } else {
                        return rawAddress.trim();
                    }
                }
            ).collect(Collectors.toList());
    }

}
