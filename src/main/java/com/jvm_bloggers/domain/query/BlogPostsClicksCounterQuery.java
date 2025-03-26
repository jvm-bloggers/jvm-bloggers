package com.jvm_bloggers.domain.query;

import com.jvm_bloggers.entities.click.ClickRepository;
import com.jvm_bloggers.entities.click.PostClicksCountByIpAddress;
import com.jvm_bloggers.entities.click.PostClicksCountByUserAgent;

import io.vavr.collection.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Slf4j
@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class BlogPostsClicksCounterQuery {

    private final ClickRepository clickRepository;

    public List<PostClicksCountByIpAddress> topPostsClicksCountByIpAddress(LocalDateTime from,
                                                                           LocalDateTime to,
                                                                           int size) {
        return clickRepository.topPostsClicksCountByIpAddress(from, to, new PageRequest(0, size));
    }

    public List<PostClicksCountByUserAgent> topPostsClicksCountByUserAgent(
        LocalDateTime from, LocalDateTime to, int size) {
        return clickRepository.topPostsClicksCountByUserAgent(from, to, new PageRequest(0, size));
    }
}
