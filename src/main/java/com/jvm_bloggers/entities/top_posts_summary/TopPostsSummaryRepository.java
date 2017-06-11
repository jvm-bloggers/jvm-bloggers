package com.jvm_bloggers.entities.top_posts_summary;

import javaslang.control.Option;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TopPostsSummaryRepository extends JpaRepository<TopPostsSummary, Long> {

    Option<TopPostsSummary> findOneByYearAndMonth(int year, int month);

}
