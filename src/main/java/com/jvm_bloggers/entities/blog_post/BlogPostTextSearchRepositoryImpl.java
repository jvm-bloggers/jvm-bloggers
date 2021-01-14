package com.jvm_bloggers.entities.blog_post;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.Query;
import org.hibernate.search.jpa.FullTextEntityManager;
import org.hibernate.search.jpa.Search;
import org.hibernate.search.query.dsl.QueryBuilder;

class BlogPostTextSearchRepositoryImpl implements BlogPostTextSearchRepository {

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  @SuppressWarnings("unchecked")
  public List<BlogPost> findApprovedPostsByTagOrTitle(String searchPhrase, int page, int pageSize) {
    var fullTextEntityManager = Search.getFullTextEntityManager(entityManager);

    BooleanQuery query = new BooleanQuery.Builder()
        .add(new BooleanClause(keywordQuery(searchPhrase, fullTextEntityManager), Occur.MUST))
        .add(new BooleanClause(approvedQuery(fullTextEntityManager), Occur.MUST))
        .build();

    return (List<BlogPost>) fullTextEntityManager
        .createFullTextQuery(query, BlogPost.class)
        .setFirstResult(page * pageSize)
        .setMaxResults(pageSize)
        .getResultList();
  }

  private Query approvedQuery(FullTextEntityManager fullTextEntityManager) {
    return getQueryBuilder(fullTextEntityManager)
        .keyword()
        .onField("approved")
        .matching(true)
        .createQuery();
  }

  private Query keywordQuery(String searchPhrase, FullTextEntityManager fullTextEntityManager) {
    return getQueryBuilder(fullTextEntityManager)
        .keyword()
        .onField("tags.tag").andField("title")
        .matching(searchPhrase)
        .createQuery();
  }

  private QueryBuilder getQueryBuilder(FullTextEntityManager fullTextEntityManager) {
    return fullTextEntityManager
        .getSearchFactory()
        .buildQueryBuilder()
        .forEntity(BlogPost.class)
        .get();
  }
}
