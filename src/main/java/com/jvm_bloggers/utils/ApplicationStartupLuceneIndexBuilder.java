package com.jvm_bloggers.utils;

import com.google.common.base.Stopwatch;
import java.util.concurrent.TimeUnit;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.search.jpa.FullTextEntityManager;
import org.hibernate.search.jpa.Search;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ApplicationStartupLuceneIndexBuilder implements ApplicationListener<ApplicationReadyEvent> {

  private final EntityManagerFactory entityManagerFactory;

  public ApplicationStartupLuceneIndexBuilder(EntityManagerFactory entityManagerFactory) {
    this.entityManagerFactory = entityManagerFactory;
  }

  @Override
  public void onApplicationEvent(ApplicationReadyEvent event) {
    EntityManager entityManager = entityManagerFactory.createEntityManager();
    Stopwatch stopwatch = Stopwatch.createStarted();
    try {
      FullTextEntityManager fullTextEntityManager = Search.getFullTextEntityManager(entityManager);
      log.info("Start creating lucene indices");
      fullTextEntityManager.createIndexer().startAndWait();
    } catch (InterruptedException e) {
      log.error("indexing error ", e);
    } finally {
      var elapsed = stopwatch.stop().elapsed(TimeUnit.MILLISECONDS);
      log.info("Finish startup indexing after {} ms ", elapsed);
      entityManager.close();
    }
  }
}
