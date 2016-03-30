package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream;

import com.google.common.annotations.VisibleForTesting;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender;
import pl.tomaszdziurko.jvm_bloggers.metadata.Metadata;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackEvent;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackMailGenerator;

import rx.Observable;
import rx.Scheduler;
import rx.schedulers.Schedulers;
import rx.subjects.PublishSubject;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

/**
 * @author Adam Dec
 */
@Slf4j
@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BruteForceAttackEventStreamManager {

    @VisibleForTesting
    public static final int MAILING_TIME_THROTTLE_IN_MINUTES = 1;

    private final ConcurrentMap<String, BruteForceAttackEventStream> streamMap =
        new ConcurrentHashMap<>();

    private final MailSender mailSender;

    private final BruteForceAttackMailGenerator bruteForceAttackMailGenerator;

    private final Scheduler scheduler;

    private final MetadataRepository metadataRepository;

    private String adminEmailAddress;

    /**
     * Builds and event stream which will emit items on predefined sampling time, event is
     * mapped to Tuple and published to subscribers.
     * <ul>
     * <li>Please note that we are using a map of streams here to have dedicated streams
     * per IP address.</li>
     * <li>Please note that onBackpressureLatest() seems to be redundant here but it is
     * left only for case when our subscriber will consume slower than we produce.</li>
     * </ul>
     *
     * @param clientAddress An IP address of the user that tries to attack us :)
     * @return An event stream that subscribers can subscribe on
     */
    public BruteForceAttackEventStream createEventStreamFor(final String clientAddress) {
        return streamMap.computeIfAbsent(clientAddress, key -> {
            final PublishSubject<BruteForceAttackEvent> subject = PublishSubject.create();
            final Observable<Pair<String, String>> observable =
                subject.sample(MAILING_TIME_THROTTLE_IN_MINUTES, TimeUnit.MINUTES, scheduler)
                    .map(this::buildTuple)
                    .observeOn(Schedulers.io())
                    .onBackpressureLatest();
            final BruteForceAttackEventStream stream =
                new BruteForceAttackEventStream(subject, observable);
            stream
                .subscribe(new BruteForceLoginAttackMailSubscriber(mailSender, adminEmailAddress));
            return stream;
        });
    }

    @PostConstruct
    public void init() {
        final Metadata adminEmailAddressMetadata =
            metadataRepository.findByName(MetadataKeys.ADMIN_EMAIL);
        if (adminEmailAddressMetadata == null) {
            throw new RuntimeException(
                MetadataKeys.ADMIN_EMAIL + " not found in Metadata table");
        }
        this.adminEmailAddress = adminEmailAddressMetadata.getValue();
        log.debug("AdminEmailAddress={}", adminEmailAddress);
    }

    @PreDestroy
    public void destroy() {
        streamMap.forEach((ipAddress, stream) -> {
            stream.terminate();
            log.debug("Terminated stream for ipAddress={}", ipAddress);
        });
        streamMap.clear();
    }

    private Pair<String, String> buildTuple(final BruteForceAttackEvent event) {
        final String mailContent = bruteForceAttackMailGenerator.prepareMailContent(event);
        log.debug("Mail content\n{}", mailContent);

        final String mailTitle = bruteForceAttackMailGenerator.prepareMailTitle(event);
        log.debug("Mail title\n{}", mailTitle);

        return new ImmutablePair<>(mailTitle, mailContent);
    }
}
