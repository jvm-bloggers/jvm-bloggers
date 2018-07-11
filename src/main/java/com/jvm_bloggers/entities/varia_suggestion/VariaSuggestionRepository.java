package com.jvm_bloggers.entities.varia_suggestion;

import io.vavr.collection.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface VariaSuggestionRepository extends JpaRepository<VariaSuggestion, Long> {

    long countByReadFalseOrReadNull();

    List<VariaSuggestion> findByReadFalseOrReadNull(Pageable pageable);
}
