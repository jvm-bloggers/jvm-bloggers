package com.jvm_bloggers.utils;

import javax.annotation.PostConstruct;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.search.jpa.Search;
import org.hibernate.search.jpa.FullTextEntityManager;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@Slf4j
public class LuceneIndexService {

  private final FullTextEntityManager fullTextEntityManager;

  public LuceneIndexService(EntityManagerFactory entityManagerFactory) {
    this.fullTextEntityManager = Search.getFullTextEntityManager(entityManagerFactory.createEntityManager());
  }

  @PostConstruct
  @Transactional
  public void triggerIndexing() {
    try {
      log.info("Start creating lucene indices");
      fullTextEntityManager.createIndexer().startAndWait();
    } catch (InterruptedException e) {
      log.error("indexing error ", e);
    }
  }
}
