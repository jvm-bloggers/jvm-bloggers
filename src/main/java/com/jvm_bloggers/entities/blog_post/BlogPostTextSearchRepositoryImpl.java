package com.jvm_bloggers.entities.blog_post;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import lombok.extern.slf4j.Slf4j;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.SortField.Type;
import org.hibernate.search.jpa.FullTextEntityManager;
import org.hibernate.search.jpa.Search;
import org.hibernate.search.query.dsl.QueryBuilder;

@Slf4j
class BlogPostTextSearchRepositoryImpl implements BlogPostTextSearchRepository {

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  @SuppressWarnings("unchecked")
  public List<BlogPost> findByTagOrTitle(String searchPhrase, int page, int pageSize) {
    var fullTextEntityManager = Search.getFullTextEntityManager(entityManager);

    var query = getQueryBuilder(fullTextEntityManager)
        .keyword()
        .onField("tags.tag").andField("title")
        .matching(searchPhrase)
        .createQuery();

    return (List<BlogPost>) fullTextEntityManager
        .createFullTextQuery(query, BlogPost.class)
        .setFirstResult(page * pageSize)
        .setMaxResults(pageSize)
        .getResultList();
  }

  private QueryBuilder getQueryBuilder(FullTextEntityManager fullTextEntityManager) {
    return fullTextEntityManager
        .getSearchFactory()
        .buildQueryBuilder()
        .forEntity(BlogPost.class)
        .get();
  }
}
