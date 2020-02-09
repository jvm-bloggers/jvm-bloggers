package com.jvm_bloggers.domain.query.unread_varia_suggestion;

import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestionRepository;

import io.vavr.collection.List;
import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class UnreadVariaSuggestionQuery {

    private final VariaSuggestionRepository variaSuggestionRepository;

    public List<UnreadVariaSuggestion> findUnreadSuggestions(int page, int size) {
        return variaSuggestionRepository.findByReadFalseOrReadNull(PageRequest.of(page, size))
            .map(UnreadVariaSuggestion::from);
    }

    public long countUnread() {
        return variaSuggestionRepository.countByReadFalseOrReadNull();
    }
}
