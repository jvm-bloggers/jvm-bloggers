package com.jvm_bloggers.entities.blog_post;

import lombok.RequiredArgsConstructor;

import org.hibernate.search.engine.search.query.dsl.SearchQueryOptionsStep;
import org.hibernate.search.mapper.orm.Search;
import org.hibernate.search.mapper.orm.search.loading.dsl.SearchLoadingOptionsStep;
import org.hibernate.search.mapper.orm.session.SearchSession;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import javax.persistence.EntityManager;

@RequiredArgsConstructor
class BlogPostTextSearchRepositoryImpl implements BlogPostTextSearchRepository {

    private final EntityManager entityManager;

    @Transactional(readOnly = true)
    @Override
    public List<BlogPost> findApprovedPostsByTagOrTitle(final String searchPhrase, final int page, final int pageSize) {
        final var session = Search.session(entityManager);

        return createQuery(searchPhrase, session)
            .sort(f -> f.field("publishedDate").desc())
            .fetchHits(page * pageSize, pageSize);
    }

    @Transactional(readOnly = true)
    @Override
    public long countApprovedPostsByTagOrTitle(final String searchPhrase) {
        final var session = Search.session(entityManager);

        return createQuery(searchPhrase, session)
            .fetchTotalHitCount();
    }

    private static SearchQueryOptionsStep<?, BlogPost, SearchLoadingOptionsStep, ?, ?> createQuery(
        final String searchPhrase,
        final SearchSession session) {

        return session.search(BlogPost.class)
            .where(f -> f.bool(b -> b.must(f.matchAll())
                .must(f.match().fields("tags.tag", "title").matching(searchPhrase))
                .must(f.match().field("approved").matching(true))));
    }
}
