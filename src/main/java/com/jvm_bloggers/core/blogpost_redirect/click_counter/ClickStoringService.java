package com.jvm_bloggers.core.blogpost_redirect.click_counter;

import com.jvm_bloggers.entities.click.Click;
import com.jvm_bloggers.entities.click.ClickRepository;
import com.jvm_bloggers.utils.NowProvider;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@RequiredArgsConstructor
@Service
public class ClickStoringService {

    private final ClickRepository clickRepository;
    private final NowProvider nowProvider;

    @Transactional
    public void persistClick(SingleClick click) {
        log.debug("Storing click for " + click.getBlogPost().getUrl());
        clickRepository.save(createClick(click));
    }

    private Click createClick(SingleClick clickEvent) {
        return new Click(
            clickEvent.getBlogPost(),
            nowProvider.now(),
            clickEvent.getIp().getValue(),
            clickEvent.getReferer().getValue(),
            clickEvent.getUserAgent().getValue()
        );
    }

}
