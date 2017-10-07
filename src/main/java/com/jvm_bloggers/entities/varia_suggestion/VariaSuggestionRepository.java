package com.jvm_bloggers.entities.varia_suggestion;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface VariaSuggestionRepository extends JpaRepository<VariaSuggestion, Long> {
}
