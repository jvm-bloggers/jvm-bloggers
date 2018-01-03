package com.jvm_bloggers.domain.command.mark_varia_suggestion;

import com.jvm_bloggers.domain.command.CommandHandler;
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestion;
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestionRepository;

import lombok.RequiredArgsConstructor;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class MarkVariaSuggestionCommandHandler implements CommandHandler<MarkVariaSuggestion> {

    private final VariaSuggestionRepository variaSuggestionRepository;

    @Override
    @EventListener
    @Transactional
    public void handle(MarkVariaSuggestion command) {
        VariaSuggestion variaSuggestion = variaSuggestionRepository.findOne(command.getId());
        variaSuggestion.setRead(true);
        variaSuggestionRepository.save(variaSuggestion);
    }
}
