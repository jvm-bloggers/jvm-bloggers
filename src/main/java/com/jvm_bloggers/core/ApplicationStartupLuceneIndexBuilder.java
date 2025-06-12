package com.jvm_bloggers.core;

import com.google.common.base.Stopwatch;

import lombok.extern.slf4j.Slf4j;

import org.hibernate.search.mapper.orm.Search;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.concurrent.TimeUnit;

import javax.persistence.EntityManagerFactory;

@ConditionalOnProperty(prefix = "lucene.index-builder", name = "enabled", havingValue = "true")
@Component
@Slf4j
public class ApplicationStartupLuceneIndexBuilder
    implements ApplicationListener<ApplicationReadyEvent> {

    private final EntityManagerFactory entityManagerFactory;

    public ApplicationStartupLuceneIndexBuilder(EntityManagerFactory entityManagerFactory) {
        this.entityManagerFactory = entityManagerFactory;
    }

    @Transactional(readOnly = true)
    @Override
    public void onApplicationEvent(ApplicationReadyEvent event) {
        final var entityManager = entityManagerFactory.createEntityManager();
        final var stopwatch = Stopwatch.createStarted();

        try {
            final var searchSession = Search.session(entityManager);
            log.info("Start creating Lucene indices...");
            searchSession.massIndexer().startAndWait();
        } catch (Exception e) {
            log.error("Lucene indexing error!", e);
        } finally {
            final var elapsed = stopwatch.stop().elapsed(TimeUnit.MILLISECONDS);
            log.info("Finish startup indexing after '{}' ms ", elapsed);
            entityManager.close();
        }
    }
}
